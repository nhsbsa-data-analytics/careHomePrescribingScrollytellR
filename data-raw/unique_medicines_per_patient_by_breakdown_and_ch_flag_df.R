library(dplyr)
library(dbplyr)
devtools::load_all()

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the geography lookup table (Region, STP and LA)
postcode_db <- con %>%
  tbl(from = "INT615_POSTCODE_LOOKUP")

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

# Create a lazy table from the care home FACT table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Tidy care home flag, join drug information and postcode info to the FACT table
fact_db <- fact_db %>%
  mutate(CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home")) %>%
  inner_join(
    y = drug_db,
    by = c("YEAR_MONTH", "CALC_PREC_DRUG_RECORD_ID" = "RECORD_ID")
  ) %>%
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

# Join fact data to patient level dimension and add an overall column
fact_db <- fact_db %>%
  left_join(
    y = patient_db,
    copy = TRUE
  ) %>%
  mutate(OVERALL = "Overall") # dummy col

# Define the breakdowns
breakdowns <- list(
  "Overall" = "OVERALL",
  "Geographical - Region" = c("PCD_REGION_NAME", "PCD_REGION_CODE"),
  "Geographical - STP" = c("PCD_STP_NAME", "PCD_STP_CODE"),
  "Geographical - Local Authority" = c("PCD_LAD_NAME", "PCD_LAD_NAME"),
  "Demographical - Gender" = "GENDER",
  "Demographical - Age Band" = "AGE_BAND"
)

# Loop over each breakdown and aggregate
for (breakdown_name in names(breakdowns)) {
  
  # Extract the breakdown cols
  breakdown_cols <- breakdowns[[breakdown_name]]
  
  # Group the table
  tmp_db <- fact_db %>%
    group_by(
      YEAR_MONTH = as.character(YEAR_MONTH),
      BREAKDOWN = breakdown_name,
      SUB_BREAKDOWN_CODE = NA,
      SUB_BREAKDOWN_NAME = !!dplyr::sym(breakdown_cols[1]),
      CH_FLAG,
      NHS_NO
    )
  
  # If there are two columns then override the code as the second column
  if (length(breakdown_cols) == 2) {
    
    tmp_db <- tmp_db %>%
      group_by(
        SUB_BREAKDOWN_CODE = !!dplyr::sym(breakdown_cols[2]),
        .add = TRUE
      )
    
  }
  
  # Get the number of unique medicines per patient
  tmp_db <- tmp_db %>%
    summarise(UNIQUE_MEDICINES = n_distinct(CHEMICAL_SUBSTANCE_BNF_DESCR)) %>%
    ungroup()

  # Summarise across patients
  tmp_db <- tmp_db %>%
    group_by(
      YEAR_MONTH,
      BREAKDOWN,
      SUB_BREAKDOWN_CODE,
      SUB_BREAKDOWN_NAME,
      CH_FLAG
    ) %>%
    summarise(
      # Unique medicines per patient
      UNIQUE_MEDICINES_PER_PATIENT = mean(UNIQUE_MEDICINES),
      # % of patients on ten or more unique medicines
      PATIENTS_TEN_OR_MORE = n_distinct(
        ifelse(UNIQUE_MEDICINES >= 10, NHS_NO, NA)
      ),
      TOTAL_PATIENTS = n_distinct(NHS_NO)
    ) %>%
    ungroup() %>%
    mutate(
      PCT_PATIENTS_TEN_OR_MORE = PATIENTS_TEN_OR_MORE / TOTAL_PATIENTS * 100
    ) %>%
    select(-c(PATIENTS_TEN_OR_MORE, TOTAL_PATIENTS))

  # Add overall mean (average monthly per patient is the metric)
  tmp_db <- tmp_db %>%
    union_all(
      y = tmp_db %>%
        group_by(
          YEAR_MONTH = "Overall",
          BREAKDOWN,
          SUB_BREAKDOWN_CODE,
          SUB_BREAKDOWN_NAME,
          CH_FLAG
        ) %>%
        summarise(
          UNIQUE_MEDICINES_PER_PATIENT = mean(UNIQUE_MEDICINES_PER_PATIENT),
          PCT_PATIENTS_TEN_OR_MORE = mean(PCT_PATIENTS_TEN_OR_MORE)
        ) %>%
        ungroup()
    )

  # Either create the table or append to them
  if (breakdown_name == "Overall") {

    # On the first iteration initialise the table
    unique_medicines_per_patient_by_breakdown_and_ch_flag_db <- tmp_db
    
  } else {

    # Union results to initialised table
    unique_medicines_per_patient_by_breakdown_and_ch_flag_db <- union_all(
      x = unique_medicines_per_patient_by_breakdown_and_ch_flag_db,
      y = tmp_db
    )
  }
}

# Collect and format for highcharter
unique_medicines_per_patient_by_breakdown_and_ch_flag_df <-
  unique_medicines_per_patient_by_breakdown_and_ch_flag_db %>%
  collect() %>%
  careHomePrescribingScrollytellR::format_data_raw("CH_FLAG")


# Add to data-raw/
usethis::use_data(
  unique_medicines_per_patient_by_breakdown_and_ch_flag_df,
  overwrite = TRUE
)

# Disconnect from database
DBI::dbDisconnect(con)
