# library
library(dbplyr)
library(dplyr)
devtools::load_all()

con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the item level base table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Calculate metric on required BNF level
get_drug_metric <- function(drug_level, metric) {

  # Column name for output df
  col_name <- as_label(enquo(drug_level))

  # Final metric name for output df
  metric_name <- paste0(as_label(enquo(metric)), "_PPM")

  # Select top_n drug-level types per ch_flag
  drug <- fact_db %>%
    group_by(CH_FLAG, {{ drug_level }}) %>%
    summarise(VALUE = sum({{ metric }})) %>%
    slice_max(VALUE, n = 20) %>%
    ungroup() %>%
    select({{ drug_level }}) %>%
    distinct() %>%
    mutate(TMP = 1)

  # Permutation of nhs_no, year_month and drug_level
  pat_year_month_drug_perm <- fact_db %>%
    select(
      CH_FLAG,
      NHS_NO,
      YEAR_MONTH
    ) %>%
    distinct() %>%
    mutate(TMP = 1) %>%
    full_join(drug, by = "TMP")

  # Pat items
  pat_items <- fact_db %>%
    group_by(
      CH_FLAG,
      NHS_NO,
      YEAR_MONTH,
      {{ drug_level }}
    ) %>%
    summarise(VALUE = sum({{ metric }})) %>%
    ungroup() %>%
    mutate(VALUE = ifelse(
      metric_name == "ITEM_PAY_DR_NIC_PPM", VALUE * 0.01, VALUE
    ))

  # Left join then calculate
  pat_year_month_drug_perm %>%
    left_join(pat_items) %>%
    mutate(VALUE = ifelse(is.na(VALUE), 0, VALUE)) %>%
    group_by(CH_FLAG, {{ drug_level }}, NHS_NO) %>%
    summarise(VALUE = mean(VALUE)) %>%
    ungroup() %>%
    group_by(CH_FLAG, {{ drug_level }}) %>%
    summarise(VALUE = mean(VALUE)) %>%
    ungroup() %>%
    rename(SUB_BNF_LEVEL_NAME := {{ drug_level }}) %>%
    mutate(BNF_LEVEL = col_name)
}

# Generate data: item_count
items_output <- get_drug_metric(CHAPTER_DESCR, ITEM_COUNT) %>%
  union_all(
    get_drug_metric(SECTION_DESCR, ITEM_COUNT)
  ) %>%
  union_all(
    get_drug_metric(PARAGRAPH_DESCR, ITEM_COUNT)
  ) %>%
  union_all(
    get_drug_metric(CHEMICAL_SUBSTANCE_BNF_DESCR, ITEM_COUNT)
  ) %>%
  collect() %>%
  mutate(
    METRIC = "ITEMS",
    BNF_LEVEL = recode(BNF_LEVEL,
      "CHAPTER_DESCR" = "Chapter",
      "SECTION_DESCR" = "Section",
      "PARAGRAPH_DESCR" = "Paragraph",
      "CHEMICAL_SUBSTANCE_BNF_DESCR" = "Chemical Substance"
    )
  )

# Generate data: drug_cost
cost_output <- get_drug_metric(CHAPTER_DESCR, ITEM_PAY_DR_NIC) %>%
  union_all(
    get_drug_metric(SECTION_DESCR, ITEM_PAY_DR_NIC)
  ) %>%
  union_all(
    get_drug_metric(PARAGRAPH_DESCR, ITEM_PAY_DR_NIC)
  ) %>%
  union_all(
    get_drug_metric(CHEMICAL_SUBSTANCE_BNF_DESCR, ITEM_PAY_DR_NIC)
  ) %>%
  collect() %>%
  mutate(
    METRIC = "COST",
    BNF_LEVEL = recode(BNF_LEVEL,
      "CHAPTER_DESCR" = "Chapter",
      "SECTION_DESCR" = "Section",
      "PARAGRAPH_DESCR" = "Paragraph",
      "CHEMICAL_SUBSTANCE_BNF_DESCR" = "Chemical Substance"
    )
  )

metrics_by_bnf_and_ch_flag_df <- items_output %>%
  union_all(cost_output)


metrics_by_bnf_and_ch_flag_df <- metrics_by_bnf_and_ch_flag_df %>%
  tidyr::pivot_wider(
    names_from = CH_FLAG,
    values_from = VALUE
  ) %>%
  rename(
    CH_VALUE = `Care home`,
    NON_CH_VALUE = `Non-care home`
  ) %>%
  mutate(
    SDC_CH_VALUE = janitor::round_half_up(CH_VALUE, 2),
    SDC_NON_CH_VALUE = janitor::round_half_up(NON_CH_VALUE, 2)
  )


metrics_by_bnf_and_ch_flag_df <- metrics_by_bnf_and_ch_flag_df %>%
  # rename(CH_VALUE = PPM_CH,
  #        NON_CH_VALUE = PPM_NON_CH,
  #        SDC_CH_VALUE = SDC_PPM_CH,
  #        SDC_NON_CH_VALUE = SDC_PPM_NON_CH) %>%
  relocate(SUB_BNF_LEVEL_NAME, .after = BNF_LEVEL)


# Use this
usethis::use_data(metrics_by_bnf_and_ch_flag_df, overwrite = TRUE)
# usethis::use_data(cost_ppm, overwrite = TRUE)

# Disconnect
DBI::dbDisconnect(con)
