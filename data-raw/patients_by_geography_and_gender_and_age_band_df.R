# Load libraries
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the geography lookup table (Region, STP and LA)
postcode_db <- con %>%
  tbl(from = in_schema("ADAIV", "INT615_PCD_REF"))

# Subset the columns
postcode_db <- postcode_db %>%
  select(POSTCODE, PCD_REGION_NAME, PCD_STP_NAME, PCD_LAD_NAME)

# Create a lazy table from the care home FACT table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Filter to care home only, join the postcode info and add gender and age band 
# groups to the FACT table
fact_db <- fact_db %>%
  filter(CH_FLAG == 1) %>%
  left_join(
    y = postcode_db, 
    by = c("PCD_NO_SPACES" = "POSTCODE")
  ) %>%
  mutate(
    PDS_GENDER = case_when(
      PDS_GENDER == 1 ~ "Male",
      PDS_GENDER == 2 ~ "Female",
      TRUE ~ NA_character_
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
    YEAR_MONTH = ifelse(is.na(YEAR_MONTH), "Overall", as.character(YEAR_MONTH)),
    GEOGRAPHY = "Overall",
    LEVEL = "Overall"
  ) %>%
  relocate(LEVEL, GEOGRAPHY, YEAR_MONTH) %>%
  arrange(YEAR_MONTH, PDS_GENDER, AGE_BAND) %>%
  collect() %>%
  # Format for highcharter
  mutate(YEAR_MONTH = forcats::fct_relevel(YEAR_MONTH, "Overall")) %>%
  arrange(YEAR_MONTH)


# Process for Region (48,608 records could not match with region)
patients_by_region_and_gender_and_age_band_df <-
  union_all(
    # Overall
    x = fact_db %>%
      group_by(PDS_GENDER, AGE_BAND, GEOGRAPHY = PCD_REGION_NAME) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup(),
    # By year month
    y = fact_db %>%
      group_by(
        YEAR_MONTH, 
        PDS_GENDER, 
        AGE_BAND, 
        GEOGRAPHY = PCD_REGION_NAME
      ) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup()
  ) %>%
  mutate(
    YEAR_MONTH = ifelse(is.na(YEAR_MONTH), "Overall", as.character(YEAR_MONTH)),
    LEVEL = "Region"
  ) %>%
  relocate(LEVEL, GEOGRAPHY, YEAR_MONTH) %>%
  arrange(YEAR_MONTH, LEVEL, GEOGRAPHY, PDS_GENDER, AGE_BAND) %>%
  collect() %>%
  # Format for highcharter
  mutate(YEAR_MONTH = forcats::fct_relevel(YEAR_MONTH, "Overall")) %>%
  arrange(YEAR_MONTH)

# Process for STP 
patients_by_stp_and_gender_and_age_band_df <-
  union_all(
    # Overall
    x = fact_db %>%
      group_by(PDS_GENDER, AGE_BAND, GEOGRAPHY = PCD_STP_NAME) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup(),
    # By year month
    y = fact_db %>%
      group_by(
        YEAR_MONTH, 
        PDS_GENDER, 
        AGE_BAND, 
        GEOGRAPHY = PCD_STP_NAME
      ) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup()
  ) %>%
  mutate(
    YEAR_MONTH = ifelse(is.na(YEAR_MONTH), "Overall", as.character(YEAR_MONTH)),
    LEVEL = "STP"
  ) %>%
  relocate(LEVEL, GEOGRAPHY, YEAR_MONTH) %>%
  arrange(YEAR_MONTH, LEVEL, GEOGRAPHY, PDS_GENDER, AGE_BAND) %>%
  collect() %>%
  # Format for highcharter
  mutate(YEAR_MONTH = forcats::fct_relevel(YEAR_MONTH, "Overall")) %>%
  arrange(YEAR_MONTH)

# Process for LA 
patients_by_la_and_gender_and_age_band_df <-
  union_all(
    # Overall
    x = fact_db %>%
      group_by(PDS_GENDER, AGE_BAND, GEOGRAPHY = PCD_LAD_NAME) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup(),
    # By year month
    y = fact_db %>%
      group_by(
        YEAR_MONTH, 
        PDS_GENDER, 
        AGE_BAND, 
        GEOGRAPHY = PCD_LAD_NAME
      ) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup()
  ) %>%
  mutate(
    YEAR_MONTH = ifelse(is.na(YEAR_MONTH), "Overall", as.character(YEAR_MONTH)),
    LEVEL = "Local Authority"
  ) %>%
  relocate(YEAR_MONTH) %>%
  arrange(YEAR_MONTH, LEVEL, GEOGRAPHY, PDS_GENDER, AGE_BAND) %>%
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
