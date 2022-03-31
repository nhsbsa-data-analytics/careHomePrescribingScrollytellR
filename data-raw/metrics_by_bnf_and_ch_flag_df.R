# Load libraries
library(dplyr)
library(dbplyr)
devtools::load_all()

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP") # strange as they don't work anymore....

# Create a lazy table from the item level base table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Loop over each bnf and aggregate
for (bnf_name in names(careHomePrescribingScrollytellR::bnfs)) {

  # Extract the bnf col
  bnf_col <- careHomePrescribingScrollytellR::bnfs[[bnf_name]]

  # Group the table
  tmp_db <- fact_db %>%
    group_by(
      BNF_LEVEL = bnf_name,
      SUB_BNF_LEVEL_NAME = .data[[bnf_col]],
      CH_FLAG,
    )

  # Sum the items and cost and unique patients
  tmp_db <- tmp_db %>%
    summarise(
      TOTAL_ITEMS = sum(ITEM_COUNT),
      TOTAL_COST = sum(ITEM_PAY_DR_NIC * 0.01),
      TOTAL_PATIENTS = n_distinct(NHS_NO)
    ) %>%
    ungroup()

  # Either create the table or append to it
  if (bnf_name == "Chapter") {
    # On the first iteration initialise the table

    metrics_by_bnf_and_ch_flag_perc_db <- tmp_db
  } else {
    # Union results to initialised table

    metrics_by_bnf_and_ch_flag_perc_db <- union_all(
      x = metrics_by_bnf_and_ch_flag_perc_db,
      y = tmp_db
    )
  }
}

# Pivot the data longer
metrics_by_bnf_and_ch_flag_perc_db <- metrics_by_bnf_and_ch_flag_perc_db %>%
  tidyr::pivot_longer(
    cols = starts_with("TOTAL_"),
    names_to = "METRIC",
    names_prefix = "TOTAL_",
    values_to = "TOTAL"
  )

# Join on the totals
metrics_by_bnf_and_ch_flag_perc_db <- metrics_by_bnf_and_ch_flag_perc_db %>%
  inner_join(
    y = fact_db %>%
      group_by(CH_FLAG) %>%
      summarise(
        TOTAL_ITEMS = sum(ITEM_COUNT),
        TOTAL_COST = sum(ITEM_PAY_DR_NIC * 0.01),
        TOTAL_PATIENTS = n_distinct(NHS_NO)
      ) %>%
      ungroup() %>%
      tidyr::pivot_longer(
        cols = starts_with("TOTAL_"),
        names_to = "METRIC",
        names_prefix = "TOTAL_",
        values_to = "OVERALL_TOTAL"
      ),
    by = c("CH_FLAG", "METRIC")
  )

# Calculate the percentages and drop the overall total column
metrics_by_bnf_and_ch_flag_perc_db <- metrics_by_bnf_and_ch_flag_perc_db %>%
  mutate(PCT = TOTAL / OVERALL_TOTAL * 100) # %>%
select(-OVERALL_TOTAL)

# Collect
metrics_by_bnf_and_ch_flag_perc_df <- metrics_by_bnf_and_ch_flag_perc_db %>%
  collect()

# Get all the possible combinations
metrics_by_bnf_and_ch_flag_perc_df <- metrics_by_bnf_and_ch_flag_perc_df %>%
  tidyr::complete(
    # Only BNF names that already exist
    tidyr::nesting(BNF_LEVEL, SUB_BNF_LEVEL_NAME),
    # Every metric and CH flag
    METRIC,
    CH_FLAG,
    fill = list(
      TOTAL = 0L,
      PCT = NA_real_
    )
  )

# Apply SDC to the metrics based on the total patients
metrics_by_bnf_and_ch_flag_perc_df <- metrics_by_bnf_and_ch_flag_perc_df %>%
  group_by(BNF_LEVEL, SUB_BNF_LEVEL_NAME, CH_FLAG) %>%
  mutate(
    SDC = max(ifelse(METRIC == "PATIENTS" & TOTAL %in% c(1, 2, 3, 4), 1, 0))
  ) %>%
  ungroup() %>%
  mutate(
    SDC_PCT = case_when(
      SDC == 1 ~ NA_real_,
      TRUE ~ janitor::round_half_up(PCT, 1),
    )
  ) %>%
  select(-SDC)

# Pivot wider by care home flag
metrics_by_bnf_and_ch_flag_perc_df <- metrics_by_bnf_and_ch_flag_perc_df %>%
  mutate(CH_FLAG = ifelse(CH_FLAG == "Care home", "CH", "NON_CH")) %>%
  tidyr::pivot_longer(cols = c(TOTAL, PCT, SDC_PCT)) %>%
  tidyr::pivot_wider(
    id_cols = BNF_LEVEL:CH_FLAG,
    names_from = c(name, CH_FLAG),
    values_from = value,
    values_fill = 0
  )

metrics_by_bnf_and_ch_flag_perc_df <- metrics_by_bnf_and_ch_flag_perc_df %>%
  mutate(METRIC = recode(METRIC, "COST" = "COST_PERC", "ITEMS" = "ITEMS_PERC")) %>%
  rename(
    CH_VALUE = PCT_CH,
    SDC_CH_VALUE = SDC_PCT_CH,
    NON_CH_VALUE = PCT_NON_CH,
    SDC_NON_CH_VALUE = SDC_PCT_NON_CH
  ) %>%
  relocate(SDC_CH_VALUE, .after = NON_CH_VALUE) # later this requires to slice to max 20

# Add to data-raw/
usethis::use_data(metrics_by_bnf_and_ch_flag_perc_df, overwrite = TRUE)
# Disconnect from database
DBI::dbDisconnect(con)
