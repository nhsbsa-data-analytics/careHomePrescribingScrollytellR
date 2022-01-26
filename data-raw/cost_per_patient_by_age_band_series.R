# Load libraries
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the item level base table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Filter to care home only
fact_db <- fact_db %>%
  filter(CH_FLAG == "Care home")

# Get data for boxplot
cost_per_patient_by_age_band_db <- fact_db %>%
  # Year month level
  group_by(YEAR_MONTH, AGE_BAND, NHS_NO) %>%
  summarise(TOTAL_COST = sum(ITEM_PAY_DR_NIC * 0.01)) %>%
  ungroup() %>%
  select(-c(YEAR_MONTH, NHS_NO))

# Collect and change factor-levels
cost_per_patient_by_age_band_df <- cost_per_patient_by_age_band_db %>%
  collect() %>%
  mutate(
    AGE_BAND = factor(
      AGE_BAND,
      levels = c("65-69", "70-74", "75-79", "80-84", "85-89", "90+")
    )
  ) %>%
  arrange(AGE_BAND)

# Format series for highcharter
cost_per_patient_by_age_band_series <- highcharter::data_to_boxplot(
  data = cost_per_patient_by_age_band_df,
  group_var = AGE_BAND,
  variable = TOTAL_COST,
  add_outliers = FALSE,
  name = "Cost per patient"
)

# Add to data-raw/
usethis::use_data(cost_per_patient_by_age_band_series, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)