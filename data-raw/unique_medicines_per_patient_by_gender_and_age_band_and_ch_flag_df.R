library(dplyr)
library(dbplyr)
devtools::load_all()

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the care home FACT table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))


# Create a lazy table from the year month table
year_month_db <- con %>%
  tbl(from = in_schema("DALL_REF", "YEAR_MONTH_DIM"))

# Filter to 2020/2021
year_month_db <- year_month_db %>%
  filter(FINANCIAL_YEAR == "2020/2021") %>%
  select(YEAR_MONTH)

# Create a lazy table from the drug DIM table
drug_db <- con %>%
  tbl(from = in_schema("DIM", "CDR_EP_DRUG_BNF_DIM"))

# Filter to BNF chapters 1 4 and 6 - 10 in 2020/2021 and subset columns
drug_db <- drug_db %>%
  filter(BNF_CHAPTER %in% c(01, 02, 03, 04, 06, 07, 08, 09, 10)) %>%
  inner_join(year_month_db) %>%
  select(YEAR_MONTH, RECORD_ID, CHEMICAL_SUBSTANCE_BNF_DESCR)

# Tidy care home flag
fact_db <- fact_db %>%
  mutate(CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home")) %>%
  inner_join(
    y = drug_db,
    by = c("YEAR_MONTH", "CALC_PREC_DRUG_RECORD_ID" = "RECORD_ID")
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
# also add drug dimension
fact_db <- fact_db %>%
  left_join(
    y = patient_db,
    copy = TRUE
  )

# Monthly unique itmes per patient by care home flag

unique_medicines_per_patient_by_gender_and_age_band_and_ch_flag_db <- fact_db %>%
  group_by(YEAR_MONTH, GENDER, AGE_BAND, CH_FLAG, NHS_NO) %>%
  summarise(
    TOTAL_PATIENTS = n_distinct(NHS_NO),
    UNIQUE_MEDICINES = n_distinct(CHEMICAL_SUBSTANCE_BNF_DESCR)
  ) %>%
  ungroup()


# Add overall mean
unique_medicines_per_patient_by_gender_and_age_band_and_ch_flag_db <-
  unique_medicines_per_patient_by_gender_and_age_band_and_ch_flag_db %>%
  group_by(GENDER, AGE_BAND, CH_FLAG) %>%
  summarise(
    # Unique medicines per patient
    UNIQUE_MEDICINES_PER_PATIENT = mean(UNIQUE_MEDICINES),
    # % of patients on ten or more unique medicines
    PATIENTS_TEN_OR_MORE = n_distinct(
      ifelse(UNIQUE_MEDICINES >= 10, NHS_NO, NA)
    ),
    TOTAL_PATIENTS_CHAPTER_TEN = n_distinct(NHS_NO)
  ) %>%
  ungroup() %>%
  mutate(
    PCT_PATIENTS_TEN_OR_MORE =
      PATIENTS_TEN_OR_MORE / TOTAL_PATIENTS_CHAPTER_TEN * 100
  )


# Collect
unique_medicines_per_patient_by_gender_and_age_band_and_ch_flag_df <-
  unique_medicines_per_patient_by_gender_and_age_band_and_ch_flag_db %>%
  collect()

# Apply SDC based on the total patients
unique_medicines_per_patient_by_gender_and_age_band_and_ch_flag_df <-
  unique_medicines_per_patient_by_gender_and_age_band_and_ch_flag_df %>%
  mutate(
    SDC = ifelse(TOTAL_PATIENTS_CHAPTER_TEN %in% c(1, 2, 3, 4), 1, 0),
    SDC_UNIQUE_MEDICINES_PER_PATIENT =
      ifelse(SDC == 1, NA_integer_, janitor::round_half_up(UNIQUE_MEDICINES_PER_PATIENT,1)),
    SDC = ifelse(PATIENTS_TEN_OR_MORE %in% c(1, 2, 3, 4), 1, 0),
    SDC_PCT_PATIENTS_TEN_OR_MORE =
      ifelse(SDC == 1, NA_integer_, janitor::round_half_up(PCT_PATIENTS_TEN_OR_MORE))
  ) %>%
  select(-SDC)


# Format for highcharter
unique_medicines_per_patient_by_gender_and_age_band_and_ch_flag_df <-
  unique_medicines_per_patient_by_gender_and_age_band_and_ch_flag_df %>%
  careHomePrescribingScrollytellR::format_data_raw(
    c("GENDER", "AGE_BAND", "CH_FLAG")
  )

# Add to data-raw/
usethis::use_data(
  unique_medicines_per_patient_by_gender_and_age_band_and_ch_flag_df,
  overwrite = TRUE
)

# Disconnect
DBI::dbDisconnect(con)
