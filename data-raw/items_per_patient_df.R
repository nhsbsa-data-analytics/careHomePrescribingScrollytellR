# Load library
library(magrittr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the care home FACT table
fact_df <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM DALL_REF.INT615_ITEM_LEVEL_BASE")
)

# Aggregate data to return:
# Monthly average number of items per patient
# By carehome flag (CH_FLAG)
items_per_patient_df <- item_level_df %>%
  dplyr::group_by(YEAR_MONTH, CH_FLAG) %>%
  dplyr::summarise(
    TOTAL_ITEMS = dplyr::sum(ITEM_COUNT),
    TOTAL_PATIENTS = dplyr::n_distinct(NHS_NO),
    ITEMS_PER_PATIENT = dplyr::sum(ITEM_COUNT) / dplyr::n_distinct(NHS_NO)
  ) %>%
  dplyr::collect() %>%
  

# Add to data-raw/
usethis::use_data(items_per_patient_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)
