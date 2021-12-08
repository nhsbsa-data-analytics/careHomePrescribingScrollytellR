library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the geography lookup table (Region, STP and LA)
postcode_db <- con %>%
  tbl(from = in_schema("ADAIV", "INT615_PCD_REF"))

# Subset the columns
postcode_db <- postcode_db %>%
  mutate(OVERALL = NA_character_) %>% # dummy col so aggregation is easier
  select(
    PCD_NO_SPACES = POSTCODE, 
    OVERALL,
    PCD_REGION_NAME, 
    PCD_STP_NAME, 
    PCD_LAD_NAME
  )

# Create a lazy table from the care home FACT table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Filter to care home only, join the postcode info
fact_db <- fact_db %>%
  filter(CH_FLAG == 1) %>%
  left_join(
    y = postcode_db, 
    copy = TRUE
  ) 

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
for (
  geography in c("OVERALL", "PCD_REGION_NAME", "PCD_STP_NAME", "PCD_LAD_NAME")
) {
  
  # Aggregate to a temporary database table
  tmp_df <-
    
    union_all(
      # Overall
      x = fact_db %>%
        group_by(
          GEOGRAPHY = geography, 
          SUB_GEOGRAPHY = !!dplyr::sym(geography),
          GENDER, 
          AGE_BAND
        ) %>%
        summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
        ungroup(),
      # By year month
      y = fact_db %>%
        group_by(
          YEAR_MONTH, 
          GEOGRAPHY = geography, 
          SUB_GEOGRAPHY = !!dplyr::sym(geography),
          GENDER, 
          AGE_BAND
        ) %>%
        summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
        ungroup()
      ) %>%
      # Any NAs are overall figures 
      mutate(
        across(
          .cols = c(YEAR_MONTH, GEOGRAPHY, SUB_GEOGRAPHY), 
          .fns = ~ ifelse(is.na(.x), "Overall", as.character(.x))
        )
      )
  
  if (geography == "OVERALL") {
    # On the first iteration initialise the table
    patients_by_geography_and_gender_and_age_band_db <- tmp_df
    
  } else {
    # Union results to initialised table
    patients_by_geography_and_gender_and_age_band_db <- union_all(
      x = patients_by_geography_and_gender_and_age_band_db,
      y = tmp_df
    )
    
  }
  
}

# Give the GEOGRAPHY column nice names
patients_by_geography_and_gender_and_age_band_db <- 
  patients_by_geography_and_gender_and_age_band_db %>%
  mutate(
    GEOGRAPHY = case_when(
      GEOGRAPHY == "OVERALL" ~ "Overall",
      GEOGRAPHY == "PCD_REGION_NAME" ~ "Region",
      GEOGRAPHY == "PCD_STP_NAME" ~ "STP",
      GEOGRAPHY == "PCD_LAD_NAME" ~ "Local Authority"
    )
  )

# Collect and format for highcharter
patients_by_geography_and_gender_and_age_band_df <- 
  patients_by_geography_and_gender_and_age_band_db %>%
  arrange(YEAR_MONTH, GEOGRAPHY, SUB_GEOGRAPHY, GENDER, AGE_BAND) %>%
  collect() %>%
  # Format for highcharter
  mutate(
    across(
      .cols = c(YEAR_MONTH, SUB_GEOGRAPHY),
      .fns = ~ forcats::fct_relevel(.x, "Overall")
    ),
    GEOGRAPHY = forcats::fct_relevel(GEOGRAPHY, "Overall", "Region", "STP")
  ) %>%
  arrange(YEAR_MONTH, GEOGRAPHY, SUB_GEOGRAPHY)

# Add to data-raw/
usethis::use_data(
  patients_by_geography_and_gender_and_age_band_df, 
  overwrite = TRUE
)

# Disconnect from database
DBI::dbDisconnect(con)
