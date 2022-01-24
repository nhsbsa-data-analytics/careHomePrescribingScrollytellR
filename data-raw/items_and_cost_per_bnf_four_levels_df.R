# Load libraries
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the year month table
year_month_db <- con %>%
  tbl(from = in_schema("DALL_REF", "YEAR_MONTH_DIM"))

# Filter to 2020/2021 FY
year_month_db <- year_month_db %>%
  filter(FINANCIAL_YEAR == "2020/2021") %>%
  select(YEAR_MONTH)

# Create a lazy table from the drug DIM table
drug_db <- con %>%
  tbl(from = in_schema("DIM", "CDR_EP_DRUG_BNF_DIM"))

# Combine the chapter / section / paragraph and description and filter to FY
drug_db <- drug_db %>%
  inner_join(year_month_db) %>%
  mutate(
    BNF_CHAPTER = paste0("(", BNF_CHAPTER, ") ", CHAPTER_DESCR),
    BNF_SECTION = paste0("(", BNF_SECTION, ") ", SECTION_DESCR),
    BNF_PARAGRAPH = PARAGRAPH_DESCR,
    BNF_CHEM_SUB = CHEMICAL_SUBSTANCE_BNF_DESCR
  ) %>%
  select(YEAR_MONTH, RECORD_ID, BNF_CHAPTER, BNF_SECTION, BNF_PARAGRAPH, BNF_CHEM_SUB)

# Create a lazy table from the care home FACT table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Join the drug information to the FACT table
fact_db <- fact_db %>%
  inner_join(
    y = drug_db,
    by = c("YEAR_MONTH", "CALC_PREC_DRUG_RECORD_ID" = "RECORD_ID")
  )


# Dumbbell chart process

for (breakdown_name in names(careHomePrescribingScrollytellR::bnf)) {

  # Extract the breakdown cols (e.g. BNF_CHAPTER)
  breakdown_cols <- careHomePrescribingScrollytellR::bnf[[breakdown_name]]

  # Group the table by each breakdown and get items and drug cost
  tmp_db <- fact_db %>%
    group_by(
      BREAKDOWN = breakdown_name,
      BNF_NAME = !!dplyr::sym(breakdown_cols),
      CH_FLAG
    ) %>%
    summarise(
      ITEMS = sum(ITEM_COUNT),
      DRUGS = sum(ITEM_PAY_DR_NIC * 0.01)
    ) %>%
    ungroup() %>%
    tidyr::pivot_longer(
      cols = -c(CH_FLAG, BREAKDOWN, BNF_NAME),
      names_to = "METRIC",
      values_to = "SUM"
    )

  tmp_db_total <- tmp_db %>%
    group_by(METRIC, CH_FLAG) %>%
    summarise(TOTAL = sum(SUM)) %>%
    ungroup()


  tmp_db <- tmp_db %>%
    inner_join(
      y = tmp_db_total,
      by = c("METRIC", "CH_FLAG")
    )

  # Replace METRIC = PATIENTS TOTAL to total patients in 2020/21 by CH_FLAG
  tmp_db_pat <- fact_db %>%
    group_by(
      BREAKDOWN = breakdown_name,
      BNF_NAME = !!dplyr::sym(breakdown_cols),
      CH_FLAG
    ) %>%
    summarise(PATIENTS = n_distinct(NHS_NO)) %>%
    ungroup() %>%
    tidyr::pivot_longer(
      cols = -c(CH_FLAG, BREAKDOWN, BNF_NAME),
      names_to = "METRIC",
      values_to = "SUM"
    )

  tmp_db_total_pat <- fact_db %>%
    group_by(CH_FLAG) %>%
    summarise(TOTAL = n_distinct(NHS_NO))

  tmp_db_patient <- tmp_db_pat %>%
    inner_join(
      y = tmp_db_total_pat,
      by = c("CH_FLAG")
    )

  # join back to tmp_db

  tmp_db <- union_all(
    x = tmp_db,
    y = tmp_db_patient
  )


  # Calculate the percentage of each group and drop the total column
  tmp_db <- tmp_db %>%
    group_by(METRIC, CH_FLAG) %>%
    mutate(PCT = SUM / TOTAL * 100) %>%
    ungroup() %>%
    select(-c(SUM, TOTAL))

  # Pivot wider by care home flag
  tmp_db <- tmp_db %>%
    mutate(CH_FLAG = ifelse(CH_FLAG == 0L, "PCT_NON_CH", "PCT_CH")) %>%
    tidyr::pivot_wider(
      names_from = CH_FLAG,
      values_from = PCT
    )

  if (breakdown_name == "BNF Chapter") {
    items_and_cost_per_bnf_db <- tmp_db
  } else {
    items_and_cost_per_bnf_db <- union_all(
      x = items_and_cost_per_bnf_db,
      y = tmp_db
    )
  }
}


# Reorder the columns, sort and collect
items_and_cost_per_bnf_df <- items_and_cost_per_bnf_db %>%
  relocate(METRIC, BREAKDOWN, BNF_NAME, PCT_CH) %>%
  arrange(METRIC, desc(PCT_CH)) %>%
  collect()

# Apply SDC to percentage columns and drop normal columns as they aren't needed
items_and_cost_per_bnf_df <- items_and_cost_per_bnf_df %>%
  mutate(
    across(
      .cols = starts_with("PCT"),
      .fns = ~ janitor::round_half_up(.x, 1),
      .names = "SDC_{col}"
    )
  ) %>%
  select(-starts_with("PCT"))


usethis::use_data(items_and_cost_per_bnf_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)
