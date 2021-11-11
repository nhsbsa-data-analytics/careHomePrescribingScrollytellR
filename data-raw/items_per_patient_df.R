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
items_per_patient_df <- fact_db %>%
  dplyr::mutate(
    CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home")
  ) %>%
  dplyr::group_by(YEAR_MONTH, CH_FLAG) %>%
  dplyr::summarise(
    TOTAL_ITEMS = dplyr::sum(ITEM_COUNT),
    TOTAL_PATIENTS = dplyr::n_distinct(NHS_NO),
    ITEMS_PER_PATIENT = dplyr::sum(ITEM_COUNT) / dplyr::n_distinct(NHS_NO)
  ) %>%
  dplyr::ungroup()

# Add overall mean and format for highcharter
items_per_patient_df <- items_per_patient_df %>%
  dplyr::union_all(
    y = items_per_patient_df %>%
      dplyr::group_by(CH_FLAG) %>%
      dplyr::summarise(ITEMS_PER_PATIENT = mean(ITEMS_PER_PATIENT))
  ) %>%
  dplyr::arrange(YEAR_MONTH) %>%
  dplyr::collect() %>%
  dplyr::mutate(
    YEAR_MONTH = lubridate::ym(YEAR_MONTH),
    CH_FLAG = forcats::fct_rev(CH_FLAG)
  )

# Add to data-raw/
usethis::use_data(items_per_patient_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)
