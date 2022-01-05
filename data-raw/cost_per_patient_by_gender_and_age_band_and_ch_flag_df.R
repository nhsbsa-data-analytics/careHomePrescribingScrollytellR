library(dplyr)
library(dbplyr)
devtools::load_all()

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the care home FACT table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Tidy care home flag
fact_db <- fact_db %>%
  mutate(CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home"))

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
      TRUE ~ "90+")
    )

# Join fact data to patient level dimension
fact_db <- fact_db %>%
  left_join(
    y = patient_db,
    copy = TRUE
  )

# Monthly cost per patient by care home flag
cost_per_patient_by_gender_and_age_band_and_ch_flag_db <- fact_db %>%
  group_by(YEAR_MONTH, GENDER, AGE_BAND, CH_FLAG) %>%
  summarise(
    TOTAL_PATIENTS = n_distinct(NHS_NO),
    COST_PER_PATIENT = sum(ITEM_PAY_DR_NIC * 0.01) / n_distinct(NHS_NO)
  ) %>%
  ungroup()

# Add overall mean
cost_per_patient_by_gender_and_age_band_and_ch_flag_db <- 
  cost_per_patient_by_gender_and_age_band_and_ch_flag_db %>%
  group_by(GENDER, AGE_BAND, CH_FLAG) %>%
  summarise(
    TOTAL_PATIENTS = mean(TOTAL_PATIENTS), # for SDC
    COST_PER_PATIENT = mean(COST_PER_PATIENT)
  ) %>%
  ungroup()

# Collect
cost_per_patient_by_gender_and_age_band_and_ch_flag_df <-
  cost_per_patient_by_gender_and_age_band_and_ch_flag_db %>%
  collect()

# Apply SDC based on the total patients
cost_per_patient_by_gender_and_age_band_and_ch_flag_df <-
  cost_per_patient_by_gender_and_age_band_and_ch_flag_df %>%
  mutate(
    SDC = ifelse(TOTAL_PATIENTS %in% c(1, 2, 3, 4), 1, 0),
    SDC_COST_PER_PATIENT =
      ifelse(SDC == 1, NA_integer_, janitor::round_half_up(COST_PER_PATIENT))
  ) %>%
  select(-SDC)

# Format for highcharter
cost_per_patient_by_gender_and_age_band_and_ch_flag_df <-
  cost_per_patient_by_gender_and_age_band_and_ch_flag_df %>%
  careHomePrescribingScrollytellR::format_data_raw("CH_FLAG")

# Add to data-raw/
usethis::use_data(
  cost_per_patient_by_gender_and_age_band_and_ch_flag_df,
  overwrite = TRUE
)

# Disconnect
DBI::dbDisconnect(con)