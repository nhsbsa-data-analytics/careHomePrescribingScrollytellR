# Load libraries
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the geography lookup table (Region, STP and LA)
postcode_db <- con %>%
  tbl(from = "INT615_POSTCODE_LOOKUP")

# Subset the columns
postcode_db <- postcode_db %>%
  select(POSTCODE, PCD_REGION_NAME, PCD_STP_NAME, PCD_LAD_NAME)

# Create a lazy table from the care home FACT table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Filter to care home only, join the postcode info
fact_db <- fact_db %>%
  filter(CH_FLAG == 1) %>%
  left_join(
    y = postcode_db, 
    by = c("PCD_NO_SPACES" = "POSTCODE")
  ) 

# Get a single gender and age for the period
patient_db <- fact_db %>%
  group_by(NHS_NO) %>%
  summarise(
    # Gender
    MALE_COUNT = sum(ifelse(PDS_GENDER == 1, 1, 0)),
    FEMALE_COUNT = sum(ifelse(PDS_GENDER == 2, 1, 0)),
    # Take the max age
    AGE = max(CALC_AGE)
  ) %>%
  mutate(
    GENDER = case_when(
      MALE_COUNT > 0 & FEMALE_COUNT == 0 ~ "Male",
      MALE_COUNT == 0 & FEMALE_COUNT > 0 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-ends_with("_COUNT"))

# Add an age band
patient_db <- patient_db %>%
  mutate(
    AGE_BAND = case_when(
      AGE < 70 ~ "65-69",
      AGE < 75 ~ "70-74",
      AGE < 80 ~ "75-79",
      AGE < 85 ~ "80-84",
      AGE < 90 ~ "85-89",
      TRUE ~ "90+"
    )
  )

# Join fact data to patient level dimension
fact_db <- fact_db %>%
  left_join(y = patient_db, by = "NHS_NO")

# Total patients by gender and age band
patients_by_gender_and_age_band_df <-
  union_all(
    # Overall
    x = fact_db %>%
      group_by(GENDER, AGE_BAND) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup(),
    # By year month
    y = fact_db %>%
      group_by(YEAR_MONTH, GENDER, AGE_BAND) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup()
  ) %>%
  mutate(
    YEAR_MONTH = ifelse(is.na(YEAR_MONTH), "Overall", as.character(YEAR_MONTH)),
    GEOGRAPHY = "Overall",
    LEVEL = "Overall"
  ) %>%
  relocate(LEVEL, GEOGRAPHY, YEAR_MONTH) %>%
  arrange(YEAR_MONTH, GENDER, AGE_BAND) %>%
  collect() %>%
  # Format for highcharter
  mutate(YEAR_MONTH = forcats::fct_relevel(YEAR_MONTH, "Overall")) %>%
  arrange(YEAR_MONTH)


# Process for Region (48,608 records could not match with region)
patients_by_region_and_gender_and_age_band_df <-
  union_all(
    # Overall
    x = fact_db %>%
      group_by(GENDER, AGE_BAND, GEOGRAPHY = PCD_REGION_NAME) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup(),
    # By year month
    y = fact_db %>%
      group_by(YEAR_MONTH, GENDER, AGE_BAND, GEOGRAPHY = PCD_REGION_NAME) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup()
  ) %>%
  mutate(
    YEAR_MONTH = ifelse(is.na(YEAR_MONTH), "Overall", as.character(YEAR_MONTH)),
    LEVEL = "Region"
  ) %>%
  relocate(LEVEL, GEOGRAPHY, YEAR_MONTH) %>%
  arrange(YEAR_MONTH, LEVEL, GEOGRAPHY, GENDER, AGE_BAND) %>%
  collect() %>%
  # Format for highcharter
  mutate(YEAR_MONTH = forcats::fct_relevel(YEAR_MONTH, "Overall")) %>%
  arrange(YEAR_MONTH)

# Process for STP 
patients_by_stp_and_gender_and_age_band_df <-
  union_all(
    # Overall
    x = fact_db %>%
      group_by(GENDER, AGE_BAND, GEOGRAPHY = PCD_STP_NAME) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup(),
    # By year month
    y = fact_db %>%
      group_by(YEAR_MONTH, GENDER, AGE_BAND, GEOGRAPHY = PCD_STP_NAME) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup()
  ) %>%
  mutate(
    YEAR_MONTH = ifelse(is.na(YEAR_MONTH), "Overall", as.character(YEAR_MONTH)),
    LEVEL = "STP"
  ) %>%
  relocate(LEVEL, GEOGRAPHY, YEAR_MONTH) %>%
  arrange(YEAR_MONTH, LEVEL, GEOGRAPHY, GENDER, AGE_BAND) %>%
  collect() %>%
  # Format for highcharter
  mutate(YEAR_MONTH = forcats::fct_relevel(YEAR_MONTH, "Overall")) %>%
  arrange(YEAR_MONTH)

# Process for LA 
patients_by_la_and_gender_and_age_band_df <-
  union_all(
    # Overall
    x = fact_db %>%
      group_by(GENDER, AGE_BAND, GEOGRAPHY = PCD_LAD_NAME) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup(),
    # By year month
    y = fact_db %>%
      group_by(YEAR_MONTH, GENDER, AGE_BAND, GEOGRAPHY = PCD_LAD_NAME) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup()
  ) %>%
  mutate(
    YEAR_MONTH = ifelse(is.na(YEAR_MONTH), "Overall", as.character(YEAR_MONTH)),
    LEVEL = "Local Authority"
  ) %>%
  relocate(YEAR_MONTH) %>%
  arrange(YEAR_MONTH, LEVEL, GEOGRAPHY, GENDER, AGE_BAND) %>%
  collect() %>%
  # Format for highcharter
  mutate(YEAR_MONTH = forcats::fct_relevel(YEAR_MONTH, "Overall")) %>%
  arrange(YEAR_MONTH)

# Bind the rows together
patients_by_geography_and_gender_and_age_band_df <- bind_rows(
  patients_by_gender_and_age_band_df,
  patients_by_region_and_gender_and_age_band_df,
  patients_by_stp_and_gender_and_age_band_df,
  patients_by_la_and_gender_and_age_band_df
)

# Add to data-raw/
usethis::use_data(
  patients_by_geography_and_gender_and_age_band_df, 
  overwrite = TRUE
)

# Disconnect from database
DBI::dbDisconnect(con)
