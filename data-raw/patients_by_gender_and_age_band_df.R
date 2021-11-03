# Load library
library(magrittr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the care home FACT table
fact_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM DALL_REF.INT615_ITEM_LEVEL_BASE")
)

# Add gender and age band groups to the FACT table
fact_db <- fact_db %>%
  dplyr::mutate(
    PDS_GENDER = dplyr::case_when(
      PDS_GENDER == 1 ~ "Male",
      PDS_GENDER == 2 ~ "Female",
      PDS_GENDER %in% c(0, 9) ~ "Unknown",
      TRUE ~ "Error"
    ),
    AGE_BAND = dplyr::case_when(
      CALC_AGE < 70 ~ "65-69",
      CALC_AGE < 75 ~ "70-74",
      CALC_AGE < 80 ~ "75-79",
      CALC_AGE < 85 ~ "80-84",
      CALC_AGE < 90 ~ "85-89",
      TRUE ~ "90+"
    )
  )

# Total patients by gender and age band
patients_by_gender_and_age_band_df <-
  rbind(

    # Overall
    fact_db %>%
      dplyr::mutate(YEAR_MONTH = "Overall") %>%
      dplyr::group_by(YEAR_MONTH, PDS_GENDER, AGE_BAND) %>%
      dplyr::summarise(TOTAL_PATIENTS = dplyr::n_distinct(NHS_NO)) %>%
      dplyr::ungroup() %>%
      dplyr::collect(),

    # By year month
    fact_db %>%
      dplyr::group_by(YEAR_MONTH, PDS_GENDER, AGE_BAND) %>%
      dplyr::summarise(TOTAL_PATIENTS = dplyr::n_distinct(NHS_NO)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(YEAR_MONTH) %>%
      dplyr::collect()
  )

# Add to data-raw/
usethis::use_data(patients_by_gender_and_age_band_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)
