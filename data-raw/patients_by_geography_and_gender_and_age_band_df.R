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
  ) %>%
  mutate(OVERALL = "Overall") # dummy col

# Loop over each geography and aggregate
for (geography_name in names(careHomePrescribingScrollytellR::geographys)) {
  
  # Extract the geography cols
  geography_cols <-
    careHomePrescribingScrollytellR::geographys[[geography_name]]
  
  # Group the table
  tmp_db <- fact_db %>%
    group_by(
      GEOGRAPHY = geography_name,
      SUB_GEOGRAPHY_CODE = NA,
      SUB_GEOGRAPHY_NAME = !!dplyr::sym(geography_cols[1]),
      # NA if SUB_GEOGRAPHY_NAME is NA
      GENDER = ifelse(is.na(SUB_GEOGRAPHY_NAME), NA, GENDER),
      # NA if SUB_GEOGRAPHY_NAME is NA or if GENDER is NA
      AGE_BAND = ifelse(
        test = is.na(SUB_GEOGRAPHY_NAME) | is.na(GENDER), 
        yes = NA, 
        AGE_BAND
      ),
    )
  
  # If there are two columns then override the code as the second column
  if (length(geography_cols) == 2) {
    tmp_db <- tmp_db %>%
      group_by(
        SUB_GEOGRAPHY_CODE = !!dplyr::sym(geography_cols[2]),
        .add = TRUE
      )
  }
  
  # Union monthly and overall
  tmp_db <-
    union_all(
      
      # Monthly total patients
      x = tmp_db %>%
        group_by(
          YEAR_MONTH = as.character(YEAR_MONTH),
          .add = TRUE
        ) %>%
        summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
        ungroup(),
      
      # Overall total patients
      y = tmp_db %>%
        group_by(
          YEAR_MONTH = "Overall",
          .add = TRUE
        ) %>%
        summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
        ungroup()
    )
  
  # Calculate the percentage
  tmp_db <- tmp_db %>%
    group_by(across(-c(GENDER, AGE_BAND, TOTAL_PATIENTS))) %>%
    mutate(PCT_PATIENTS = TOTAL_PATIENTS / sum(TOTAL_PATIENTS) * 100) %>% 
    ungroup()
  
  # Either create the table or append to it
  if (geography_name == "Overall") {
    
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

# Collect
patients_by_geography_and_gender_and_age_band_df <-
  patients_by_geography_and_gender_and_age_band_db %>%
  collect() 

# Get all the possible combinations
patients_by_geography_and_gender_and_age_band_df <-
  patients_by_geography_and_gender_and_age_band_df %>%
  tidyr::complete(
    # Every year month
    YEAR_MONTH, 
    # Only geographies that already exist
    tidyr::nesting(GEOGRAPHY, SUB_GEOGRAPHY_CODE, SUB_GEOGRAPHY_NAME), 
    # Only age band and gender combinations that exist (so we only have NA age
    # band for NA genders)
    tidyr::nesting(AGE_BAND, GENDER),
    fill = list(TOTAL_PATIENTS = 0, PCT_PATIENTS = 0)
  ) 

# Apply SDC to total patients and percentage of patients
patients_by_geography_and_gender_and_age_band_df <-
  patients_by_geography_and_gender_and_age_band_df %>%
  mutate(
    SDC = ifelse(TOTAL_PATIENTS %in% c(1, 2, 3, 4), 1, 0),
    SDC_TOTAL_PATIENTS = 
      ifelse(SDC == 1, NA_integer_, round(TOTAL_PATIENTS, -1)),
    SDC_PCT_PATIENTS =
      ifelse(SDC == 1, NA_integer_, janitor::round_half_up(PCT_PATIENTS))
  ) %>% 
  select(-SDC)

# Format factors etc and sort
patients_by_geography_and_gender_and_age_band_df <-
  patients_by_geography_and_gender_and_age_band_df %>%
  careHomePrescribingScrollytellR::format_data_raw(
    vars = c("GENDER", "AGE_BAND")
  )

# Add to data-raw/
usethis::use_data(
  patients_by_geography_and_gender_and_age_band_df,
  overwrite = TRUE
)

# Disconnect from database
DBI::dbDisconnect(con)
