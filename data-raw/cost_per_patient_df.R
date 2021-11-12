# Load library
library(magrittr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the care home FACT table
fact_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM DALL_REF.INT615_ITEM_LEVEL_BASE")
)

# Monthly cost per patient by care home flag
cost_per_patient_df <- fact_db %>%
  dplyr::mutate(
    CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home")
  ) %>%
  dplyr::group_by(YEAR_MONTH, CH_FLAG) %>%
  dplyr::summarise(
    TOTAL_COST = sum(ITEM_PAY_DR_NIC * 0.01),
    TOTAL_PATIENTS = dplyr::n_distinct(NHS_NO),
    COST_PER_PATIENT = sum(ITEM_PAY_DR_NIC * 0.01) / dplyr::n_distinct(NHS_NO)
  ) %>%
  dplyr::ungroup()

# Add overall mean and format for highcharter
cost_per_patient_df <- cost_per_patient_df %>%
  dplyr::union_all(
    y = cost_per_patient_df %>%
      dplyr::group_by(CH_FLAG) %>%
      dplyr::summarise(COST_PER_PATIENT = mean(COST_PER_PATIENT))
  ) %>%
  dplyr::arrange(YEAR_MONTH) %>%
  dplyr::collect() %>%
  dplyr::mutate(
    YEAR_MONTH = lubridate::ym(YEAR_MONTH),
    CH_FLAG = forcats::fct_rev(CH_FLAG)
  )

# Add to data-raw/
usethis::use_data(cost_per_patient_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)
