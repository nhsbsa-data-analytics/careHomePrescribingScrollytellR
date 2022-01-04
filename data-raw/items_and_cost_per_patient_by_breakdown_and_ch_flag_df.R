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

# Tidy care home flag and join the postcode info
fact_db <- fact_db %>%
  mutate(CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home")) %>%
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

# Loop over each breakdown and aggregate
for (breakdown_name in names(careHomePrescribingScrollytellR::breakdowns)) {

  # Extract the breakdown cols
  breakdown_cols <-
    careHomePrescribingScrollytellR::breakdowns[[breakdown_name]]

  # Group the table
  tmp_db <- fact_db %>%
    group_by(
      YEAR_MONTH = as.character(YEAR_MONTH),
      BREAKDOWN = breakdown_name,
      SUB_BREAKDOWN_CODE = NA,
      SUB_BREAKDOWN_NAME = !!dplyr::sym(breakdown_cols[1]),
      CH_FLAG
    )

  # If there are two columns then override the code as the second column
  if (length(breakdown_cols) == 2) {
    tmp_db <- tmp_db %>%
      group_by(
        SUB_BREAKDOWN_CODE = !!dplyr::sym(breakdown_cols[2]),
        .add = TRUE
      )
  }

  # Monthly cost per patient by care home flag
  tmp_db <- tmp_db %>%
    summarise(
      TOTAL_ITEMS = sum(ITEM_COUNT),
      TOTAL_COST = sum(ITEM_PAY_DR_NIC * 0.01),
      TOTAL_PATIENTS = n_distinct(NHS_NO),
      ITEMS_PER_PATIENT = sum(ITEM_COUNT) / n_distinct(NHS_NO),
      COST_PER_PATIENT = sum(ITEM_PAY_DR_NIC * 0.01) / n_distinct(NHS_NO)
    ) %>%
    ungroup()

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
          TOTAL_PATIENTS = mean(TOTAL_PATIENTS), # for SDC
          ITEMS_PER_PATIENT = mean(ITEMS_PER_PATIENT),
          COST_PER_PATIENT = mean(COST_PER_PATIENT)
        ) %>%
        ungroup()
    )

  # Either create the table or append to it
  if (breakdown_name == "Overall") {

    # On the first iteration initialise the table
    items_and_cost_per_patient_by_breakdown_and_ch_flag_db <- tmp_db
    
  } else {

    # Union results to initialised table
    items_and_cost_per_patient_by_breakdown_and_ch_flag_db <- union_all(
      x = items_and_cost_per_patient_by_breakdown_and_ch_flag_db,
      y = tmp_db
    )
  }
  
}

# Collect 
items_and_cost_per_patient_by_breakdown_and_ch_flag_df <-
  items_and_cost_per_patient_by_breakdown_and_ch_flag_db %>%
  collect()

# Get all the possible combinations
items_and_cost_per_patient_by_breakdown_and_ch_flag_df <-
  items_and_cost_per_patient_by_breakdown_and_ch_flag_df %>%
  tidyr::complete(
    # Every year month
    YEAR_MONTH, 
    # Only breakdowns that already exist
    tidyr::nesting(BREAKDOWN, SUB_BREAKDOWN_CODE, SUB_BREAKDOWN_NAME), 
    # Every CH flag
    CH_FLAG,
    fill = list(TOTAL_PATIENTS = 0)
  ) 

# Apply SDC to the metrics based on the total patients
items_and_cost_per_patient_by_breakdown_and_ch_flag_df <-
  items_and_cost_per_patient_by_breakdown_and_ch_flag_df %>%
  mutate(
    SDC = ifelse(TOTAL_PATIENTS %in% c(1, 2, 3, 4), 1, 0),
    SDC_ITEMS_PER_PATIENT = 
      ifelse(SDC == 1, NA_integer_, janitor::round_half_up(ITEMS_PER_PATIENT)),
    SDC_COST_PER_PATIENT =
      ifelse(SDC == 1, NA_integer_, janitor::round_half_up(COST_PER_PATIENT))
  ) %>% 
  select(-SDC)

# Format for highcharter
items_and_cost_per_patient_by_breakdown_and_ch_flag_df <- 
  items_and_cost_per_patient_by_breakdown_and_ch_flag_df %>%
  careHomePrescribingScrollytellR::format_data_raw("CH_FLAG")

# Add to data-raw/
usethis::use_data(
  items_and_cost_per_patient_by_breakdown_and_ch_flag_df,
  overwrite = TRUE
)

# Disconnect from database
DBI::dbDisconnect(con)
