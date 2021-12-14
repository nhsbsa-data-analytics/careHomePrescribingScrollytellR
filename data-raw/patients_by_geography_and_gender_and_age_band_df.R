library(dplyr)
library(dbplyr)
devtools::load_all()

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the geography lookup table (Region, STP and LA)
postcode_db <- con %>%
  tbl(from = "INT615_POSTCODE_LOOKUP")

# Create a lazy table from the care home FACT table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Filter to care home only, join the postcode info
fact_db <- fact_db %>%
  filter(CH_FLAG == 1) %>%
  left_join(
    y = postcode_db %>% rename(PCD_NO_SPACES = POSTCODE),
    copy = TRUE
  ) %>%
  mutate(OVERALL_CODE = NA, OVERALL_NAME = "Overall") # dummy col

# Get a single gender and age for the period
patient_db <- fact_db %>%
  group_by(NHS_NO) %>%
  summarise(
    # Gender
    MALE_COUNT = sum(
      ifelse(PDS_GENDER == 1, 1, 0),
      na.rm = TRUE
    ),
    FEMALE_COUNT = sum(
      ifelse(PDS_GENDER == 2, 1, 0),
      na.rm = TRUE
    ),
    # Take the max age
    AGE = max(
      CALC_AGE,
      na.rm = TRUE
    )
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
  left_join(
    y = patient_db,
    copy = TRUE
  )

# Loop over geography cols and aggregate
for (geography in c("OVERALL", "PCD_REGION", "PCD_STP", "PCD_LAD")) {

  # Aggregate to a temporary database table
  tmp_db <-
    union_all(

      # Monthly total patients
      x = fact_db %>%
        group_by(
          YEAR_MONTH = as.character(YEAR_MONTH),
          GEOGRAPHY = switch(geography,
            "OVERALL" = "Overall",
            "PCD_REGION" = "Region",
            "PCD_STP" = "STP",
            "PCD_LAD" = "Local Authority"
          ),
          SUB_GEOGRAPHY_CODE = !!dplyr::sym(paste0(geography, "_CODE")),
          SUB_GEOGRAPHY_NAME = !!dplyr::sym(paste0(geography, "_NAME")),
          GENDER,
          AGE_BAND
        ) %>%
        summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
        ungroup(),

      # Overall total patients
      y = fact_db %>%
        group_by(
          YEAR_MONTH = "Overall",
          GEOGRAPHY = switch(geography,
            "OVERALL" = "Overall",
            "PCD_REGION" = "Region",
            "PCD_STP" = "STP",
            "PCD_LAD" = "Local Authority"
           ),
          SUB_GEOGRAPHY_CODE = !!dplyr::sym(paste0(geography, "_CODE")),
          SUB_GEOGRAPHY_NAME = !!dplyr::sym(paste0(geography, "_NAME")),
          GENDER,
          AGE_BAND
        ) %>%
        summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
        ungroup()
    )

  # Either create the table or append to it
  if (geography == "OVERALL") {

    # On the first iteration initialise the table
    patients_by_geography_and_gender_and_age_band_db <- tmp_db
  } else {

    # Union results to initialised table
    patients_by_geography_and_gender_and_age_band_db <- union_all(
      x = patients_by_geography_and_gender_and_age_band_db,
      y = tmp_db
    )
  }
}

# Collect and format for highcharter
patients_by_geography_and_gender_and_age_band_df <-
  patients_by_geography_and_gender_and_age_band_db %>%
  collect() %>%
  careHomePrescribingScrollytellR::format_data_raw(GENDER, AGE_BAND)

# Statistical Disclosure Control to suppress low numbers and round to nearest 10
patients_by_geography_and_gender_and_age_band_df <- patients_by_geography_and_gender_and_age_band_df %>%
  mutate(TOTAL_PATIENTS = case_when(
    TOTAL_PATIENTS == 0 ~ 0,
    TOTAL_PATIENTS > 0 & TOTAL_PATIENTS <= 4 ~ NA_real_,
    TOTAL_PATIENTS >= 5 & TOTAL_PATIENTS <= 9 ~ 10,
    TRUE ~ as.numeric(plyr::round_any(TOTAL_PATIENTS, 10))
  )
  )

# Replace suppressed NA value with C
patients_by_geography_and_gender_and_age_band_df$TOTAL_PATIENTS <- ifelse(
  is.na(patients_by_geography_and_gender_and_age_band_df$TOTAL_PATIENTS), "c", 
  patients_by_geography_and_gender_and_age_band_df$TOTAL_PATIENTS)

# Add to data-raw/
usethis::use_data(
  patients_by_geography_and_gender_and_age_band_df,
  overwrite = TRUE
)

# Disconnect from database
DBI::dbDisconnect(con)
