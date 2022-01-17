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

# Get the total items and cost per BNF chapter, section (level 2)
items_and_cost_per_bnf_chapter_and_section_df <- fact_db %>%
  filter(CH_FLAG == 1) %>%
  group_by(BNF_CHAPTER, BNF_SECTION) %>%
  summarise(
    Items = sum(ITEM_COUNT),
    `Drug Cost` = sum(ITEM_PAY_DR_NIC * 0.01)
  ) %>%
  ungroup() %>%
  tidyr::pivot_longer(
    cols = -c(BNF_CHAPTER, BNF_SECTION),
    names_to = "METRIC",
    values_to = "TOTAL_LEVEL_2"
  ) %>%
  collect()

# Group any BNF sections ranked 8th or less into a group called "Other"
items_and_cost_per_bnf_chapter_and_section_df <-
  items_and_cost_per_bnf_chapter_and_section_df %>%
  group_by(METRIC, BNF_CHAPTER) %>%
  mutate(
    BNF_SECTION = forcats::fct_lump(BNF_SECTION, n = 7, w = TOTAL_LEVEL_2)
  ) %>%
  ungroup() %>%
  group_by(METRIC, BNF_CHAPTER, BNF_SECTION) %>%
  summarise(TOTAL_LEVEL_2 = sum(TOTAL_LEVEL_2)) %>%
  ungroup() %>%
  # Add %s
  group_by(METRIC, BNF_CHAPTER) %>%
  mutate(PCT_LEVEL_2 = TOTAL_LEVEL_2 / sum(TOTAL_LEVEL_2) * 100) %>%
  ungroup()

# Get the total items and cost per BNF chapter (level 1)
items_and_cost_per_bnf_chapter_df <-
  items_and_cost_per_bnf_chapter_and_section_df %>%
  group_by(METRIC, BNF_CHAPTER) %>%
  summarise(TOTAL_LEVEL_1 = sum(TOTAL_LEVEL_2)) %>%
  ungroup()

# Group any BNF chapters ranked 8th or less into a group called "Other"
items_and_cost_per_bnf_chapter_df <- items_and_cost_per_bnf_chapter_df %>%
  group_by(
    METRIC,
    BNF_CHAPTER = forcats::fct_lump(BNF_CHAPTER, n = 7, w = TOTAL_LEVEL_1)
  ) %>%
  summarise(TOTAL_LEVEL_1 = sum(TOTAL_LEVEL_1)) %>%
  ungroup() %>%
  # Add %s
  group_by(METRIC) %>%
  mutate(PCT_LEVEL_1 = TOTAL_LEVEL_1 / sum(TOTAL_LEVEL_1) * 100) %>%
  ungroup()

# Join the two datasets together
items_and_cost_per_bnf_chapter_and_section_df <-
  left_join(
    x = items_and_cost_per_bnf_chapter_df,
    y = items_and_cost_per_bnf_chapter_and_section_df
  )

# Apply SDC to total and percentage columns and drop them
items_and_cost_per_bnf_chapter_and_section_df <-
  items_and_cost_per_bnf_chapter_and_section_df %>%
  mutate(
    across(
      .cols = starts_with("TOTAL"),
      .fns = ~ round(.x, digits = -3),
      .names = "SDC_{col}"
    ),
    across(
      .cols = starts_with("PCT"),
      .fns = ~ janitor::round_half_up(.x),
      .names = "SDC_{col}"
    )
  ) %>%
  select(-c(starts_with("TOTAL"), starts_with("PCT")))

# Reorder cols
items_and_cost_per_bnf_chapter_and_section_df <-
  items_and_cost_per_bnf_chapter_and_section_df %>%
  select(
    METRIC,
    BNF_CHAPTER,
    SDC_TOTAL_LEVEL_1,
    SDC_PCT_LEVEL_1,
    BNF_SECTION,
    SDC_TOTAL_LEVEL_2,
    SDC_PCT_LEVEL_2
  )

# Get the total items and cost per BNF paragraph to use for the dumbbell chart
# TODO:I will come back this later to make much efficient


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
      Items = sum(ITEM_COUNT),
      `Drug Cost` = sum(ITEM_PAY_DR_NIC * 0.01)
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


  tmp_db_top20 <- tmp_db %>%
    filter(CH_FLAG == 1) %>%
    group_by(METRIC) %>%
    slice_max(order_by = SUM, n = 20) %>%
    ungroup() %>%
    select(METRIC, BREAKDOWN, BNF_NAME)



  # inner join with tmp_db
  tmp_db <- tmp_db %>%
    inner_join(y = tmp_db_top20)


  # # Calculate the percentage of each group and drop the total column
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

# Add to data-raw/
usethis::use_data(
  items_and_cost_per_bnf_chapter_and_section_df,
  overwrite = TRUE
)
usethis::use_data(items_and_cost_per_bnf_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)
