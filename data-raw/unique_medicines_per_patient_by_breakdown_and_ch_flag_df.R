library(dplyr)
library(dbplyr)
devtools::load_all()

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the item level base table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Filter to BNF chapters 1 4 and 6 - 10 and add a dummy overall column
fact_db <- fact_db %>%
  filter(BNF_CHAPTER %in% c(01, 02, 03, 04, 06, 07, 08, 09, 10)) %>%
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
      SUB_BREAKDOWN_NAME = .data[[breakdown_cols[1]]],
      CH_FLAG,
      NHS_NO
    )

  # If there are two columns then override the code as the second column
  if (length(breakdown_cols) == 2) {
    tmp_db <- tmp_db %>%
      group_by(
        SUB_BREAKDOWN_CODE = .data[[breakdown_cols[2]]],
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
      TOTAL_PATIENTS_CHAPTER_TEN = n_distinct(NHS_NO)
    ) %>%
    ungroup() %>%
    mutate(
      PCT_PATIENTS_TEN_OR_MORE =
        PATIENTS_TEN_OR_MORE / TOTAL_PATIENTS_CHAPTER_TEN * 100
    )

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
          TOTAL_PATIENTS_CHAPTER_TEN = mean(TOTAL_PATIENTS_CHAPTER_TEN),
          UNIQUE_MEDICINES_PER_PATIENT = mean(UNIQUE_MEDICINES_PER_PATIENT),
          PATIENTS_TEN_OR_MORE = mean(PATIENTS_TEN_OR_MORE),
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

# Collect
unique_medicines_per_patient_by_breakdown_and_ch_flag_df <-
  unique_medicines_per_patient_by_breakdown_and_ch_flag_db %>%
  collect()

# Get all the possible combinations
unique_medicines_per_patient_by_breakdown_and_ch_flag_df <-
  unique_medicines_per_patient_by_breakdown_and_ch_flag_df %>%
  tidyr::complete(
    # Every year month
    YEAR_MONTH,
    # Only breakdowns that already exist
    tidyr::nesting(BREAKDOWN, SUB_BREAKDOWN_CODE, SUB_BREAKDOWN_NAME),
    # Every CH flag
    CH_FLAG,
    fill = list(
      TOTAL_PATIENTS_CHAPTER_TEN = 0,
      PATIENTS_TEN_OR_MORE = 0,
    )
  )

# Apply SDC to the metrics based on the total patients adn total patients 10 or
# more
unique_medicines_per_patient_by_breakdown_and_ch_flag_df <-
  unique_medicines_per_patient_by_breakdown_and_ch_flag_df %>%
  mutate(
    SDC = ifelse(TOTAL_PATIENTS_CHAPTER_TEN %in% c(1, 2, 3, 4), 1, 0),
    SDC_UNIQUE_MEDICINES_PER_PATIENT =
      ifelse(SDC == 1, NA_integer_, janitor::round_half_up(UNIQUE_MEDICINES_PER_PATIENT, 1)),
    SDC = ifelse(PATIENTS_TEN_OR_MORE %in% c(1, 2, 3, 4), 1, 0),
    SDC_PCT_PATIENTS_TEN_OR_MORE =
      ifelse(SDC == 1, NA_integer_, janitor::round_half_up(PCT_PATIENTS_TEN_OR_MORE))
  ) %>%
  select(-SDC)

# Format for highcharter
unique_medicines_per_patient_by_breakdown_and_ch_flag_df <-
  unique_medicines_per_patient_by_breakdown_and_ch_flag_df %>%
  careHomePrescribingScrollytellR::format_data_raw("CH_FLAG")

# Add to data-raw/
usethis::use_data(
  unique_medicines_per_patient_by_breakdown_and_ch_flag_df,
  overwrite = TRUE
)

# Disconnect from database
DBI::dbDisconnect(con)
