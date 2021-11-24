# Load library
library(magrittr)
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the care home FACT table
fact_db <- tbl(
  src = con,
  from = sql("SELECT * FROM DALL_REF.INT615_ITEM_LEVEL_BASE")
)

# Filter to care home only and add gender and age band groups to the FACT table
fact_db <- fact_db %>%
  filter(CH_FLAG == 1) %>%
  mutate(
    PDS_GENDER = case_when(
      PDS_GENDER == 1 ~ "Male",
      PDS_GENDER == 2 ~ "Female",
      PDS_GENDER %in% c(0, 9) ~ "Unknown",
      TRUE ~ "Error"
    ),
    AGE_BAND = case_when(
      CALC_AGE < 70 ~ "65-69",
      CALC_AGE < 75 ~ "70-74",
      CALC_AGE < 80 ~ "75-79",
      CALC_AGE < 85 ~ "80-84",
      CALC_AGE < 90 ~ "85-89",
      TRUE ~ "90+"
    )
  )

# Join with geography (STP, LA)

geography_lookup <- tbl(
  src = con,
  from = sql("SELECT * FROM KAYGO.INT615_PCD_REF")
)

geography_lookup <- geography_lookup %>%
  select(POSTCODE, PCD_STP_NAME, PCD_LAD_NAME) # keep only three geography of interest for the age/gender breakdown


fact_db <- fact_db %>%
  left_join(
    y = geography_lookup,
    by = c("PCD_NO_SPACES" = "POSTCODE")
  )


# Create various aggregation - Overall, By year month, By STP and year month, By LA and year month



# Total patients by gender and age band
patients_by_gender_and_age_band_df <-
  union_all(
    # Overall
    x = fact_db %>%
      group_by(PDS_GENDER, AGE_BAND) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup(),
    # By year month
    y = fact_db %>%
      group_by(YEAR_MONTH, PDS_GENDER, AGE_BAND) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup()
  ) %>%
  mutate(
    YEAR_MONTH = ifelse(is.na(YEAR_MONTH), "Overall", as.character(YEAR_MONTH))
  ) %>%
  relocate(YEAR_MONTH) %>%
  arrange(YEAR_MONTH, PDS_GENDER, AGE_BAND) %>%
  collect() %>%
  # Format for highcharter
  mutate(
    YEAR_MONTH = forcats::fct_relevel(YEAR_MONTH, "Overall"),
    GEOGRAPHY = "Overall",
    LEVEL = "Overall"
  ) %>%
  arrange(YEAR_MONTH)

# Process for STP (union all didn't work so bit messy)

patients_by_gender_and_age_band_stp_df <-
  fact_db %>%
  group_by(YEAR_MONTH, PDS_GENDER, AGE_BAND, PCD_STP_NAME) %>%
  summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
  ungroup() %>%
  arrange(YEAR_MONTH, PDS_GENDER, AGE_BAND, PCD_STP_NAME) %>%
  collect() %>%
  mutate(
    YEAR_MONTH = as.character(YEAR_MONTH),
    LEVEL = "STP"
  ) %>%
  # Format for highcharter
  rename(GEOGRAPHY = PCD_STP_NAME)

# Process for LA

patients_by_gender_and_age_band_la_df <-
  fact_db %>%
  group_by(YEAR_MONTH, PDS_GENDER, AGE_BAND, PCD_LAD_NAME) %>%
  summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
  ungroup() %>%
  arrange(YEAR_MONTH, PDS_GENDER, AGE_BAND, PCD_LAD_NAME) %>%
  collect() %>%
  mutate(
    YEAR_MONTH = as.character(YEAR_MONTH),
    LEVEL = "LA"
  ) %>%
  # Format for highcharter
  rename(GEOGRAPHY = PCD_LAD_NAME)


# rbind

patients_by_gender_and_age_band_df <- bind_rows(
  patients_by_gender_and_age_band_df,
  patients_by_gender_and_age_band_stp_df,
  patients_by_gender_and_age_band_la_df
)

# Also keep geography_lookup for the inputselect

stp_la_lookup <- geography_lookup %>%
  distinct(PCD_STP_NAME, PCD_LAD_NAME) %>%
  collect() %>%
  rename(
    STP = PCD_STP_NAME,
    LA = PCD_LAD_NAME
  ) %>%
  add_row(STP = "Overall", LA = "Overall")


# keep list of STP and LA
stp_list <- stp_la_lookup %>%
  distinct(STP)

la_list <- stp_la_lookup %>%
  distinct(LA)


# Add to data-raw/
usethis::use_data(patients_by_gender_and_age_band_df, overwrite = TRUE)
usethis::use_data(stp_list, overwrite = TRUE)
usethis::use_data(la_list, overwrite = TRUE)




# Disconnect from database
DBI::dbDisconnect(con)
