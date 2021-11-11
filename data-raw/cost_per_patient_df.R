# Load library
library(magrittr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the care home FACT table
fact_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM DALL_REF.INT615_ITEM_LEVEL_BASE")
)

# Monthly number of items per patient by care home flag
cost_per_patient_df <- fact_db %>%
  dplyr::mutate(
    CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home")
  ) %>%
  dplyr::group_by(YEAR_MONTH, CH_FLAG) %>%
  dplyr::summarise(
    TOTAL_COST = dplyr::sum(ITEM_PAY_DR_NIC * 0.01),
    TOTAL_PATIENTS = dplyr::n_distinct(NHS_NO),
    COST_PER_PATIENT = dplyr::sum(ITEM_PAY_DR_NIC * 0.01) / dplyr::n_distinct(NHS_NO),
    .groups = "drop"
  ) %>%
  dplyr::arrange(CH_FLAG, YEAR_MONTH) %>%
  dplyr::collect() %>%
  # Format columns for highcharter
  dplyr::mutate(
    YEAR_MONTH = lubridate::ym(YEAR_MONTH),
    CH_FLAG = forcats::fct_rev(CH_FLAG)
  )

# Add to data-raw/
usethis::use_data(cost_per_patient_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)
