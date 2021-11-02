# Load library
library(magrittr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the care home FACT table
fact_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM DALL_REF.INT615_ITEM_LEVEL_BASE")
)

# Number of patients of each gender by care home flag
patients_by_gender_df <- fact_db %>%
  dplyr::mutate(
    CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home"),
    PDS_GENDER = dplyr::case_when(
      PDS_GENDER == 1 ~ "Male",
      PDS_GENDER == 2 ~ "Female",
      PDS_GENDER %in% c(0, 9) ~ "Unknown",
      TRUE ~ "Error"
    )
  ) %>%
  dplyr::group_by(PDS_GENDER, CH_FLAG) %>%
  dplyr::summarise(TOTAL_PATIENTS = dplyr::n_distinct(NHS_NO)) %>% 
  dplyr::group_by(CH_FLAG) %>% 
  dplyr::mutate(PCT = TOTAL_PATIENTS / sum(TOTAL_PATIENTS)) %>% 
  dplyr::ungroup() %>%
  dplyr::collect() %>%
  # Format columns for highcharter
  dplyr::mutate(
    CH_FLAG = forcats::fct_rev(CH_FLAG),
    PDS_GENDER = forcats::fct_relevel(PDS_GENDER, "Male", "Female", "Unknown")
  )

# Add to data-raw/
usethis::use_data(patients_by_gender_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)
