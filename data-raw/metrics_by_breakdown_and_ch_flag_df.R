library(dplyr)
library(dbplyr)
devtools::load_all()

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the item level base table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Add a dummy overall column
fact_db <- fact_db %>%
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

  # Sum the items and cost per patient month
  tmp_db <- tmp_db %>%
    summarise(
      TOTAL_ITEMS = sum(ITEM_COUNT),
      TOTAL_COST = sum(ITEM_PAY_DR_NIC * 0.01),
      UNIQUE_MEDICINES = n_distinct(
        ifelse(
          test = BNF_CHAPTER %in% c(01, 02, 03, 04, 06, 07, 08, 09, 10),
          yes = CHEMICAL_SUBSTANCE_BNF_DESCR,
          no = NA_character_
        )
      )
    )
  
  # Replicate the data with an overall year month
  tmp_db <- tmp_db %>%
    union_all(
      y = tmp_db %>%
        mutate(YEAR_MONTH = "Overall")
    )

  # Calculate the metrics per patient month
  tmp_db <- tmp_db %>%
    ungroup(NHS_NO) %>%
    summarise(
      # Items and cost
      TOTAL_PATIENTS = n_distinct(NHS_NO), # For SDC
      ITEMS_PER_PATIENT_MONTH = mean(TOTAL_ITEMS),
      COST_PER_PATIENT_MONTH = mean(TOTAL_COST),
      # Unique medicines
      TOTAL_PATIENTS_UNIQUE_MEDICINES = n_distinct(
        ifelse(
          test = UNIQUE_MEDICINES > 1, 
          yes = NHS_NO, 
          NA
        )
      ),
      UNIQUE_MEDICINES_PER_PATIENT_MONTH = mean(UNIQUE_MEDICINES),
      TOTAL_PATIENTS_TEN_OR_MORE = n_distinct(
        ifelse(
          test = UNIQUE_MEDICINES >= 10, 
          yes = NHS_NO, 
          NA
        )
      )
    ) %>%
    ungroup() %>%
    mutate(
      PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH =
        TOTAL_PATIENTS_TEN_OR_MORE / TOTAL_PATIENTS_UNIQUE_MEDICINES * 100
    )

  # Either create the table or append to it
  if (breakdown_name == "Overall") {
    # On the first iteration initialise the table
    
    metrics_by_breakdown_and_ch_flag_db <- tmp_db
    
  } else {
    # Union results to initialised table
    
    metrics_by_breakdown_and_ch_flag_db <- union_all(
      x = metrics_by_breakdown_and_ch_flag_db,
      y = tmp_db
    )
    
  }
}

# Collect
metrics_by_breakdown_and_ch_flag_df <- metrics_by_breakdown_and_ch_flag_db %>%
  collect()

# Get all the possible combinations
metrics_by_breakdown_and_ch_flag_df <- metrics_by_breakdown_and_ch_flag_df %>%
  tidyr::complete(
    # Every year month
    YEAR_MONTH,
    # Only breakdowns that already exist
    tidyr::nesting(BREAKDOWN, SUB_BREAKDOWN_CODE, SUB_BREAKDOWN_NAME),
    # Every CH flag
    CH_FLAG,
    fill = list(
      TOTAL_PATIENTS = 0L,
      TOTAL_PATIENTS_UNIQUE_MEDICINES = 0L,
      TOTAL_PATIENTS_TEN_OR_MORE = 0L,
      )
  )

# Apply SDC to the metrics based on the total patients
metrics_by_breakdown_and_ch_flag_df <- metrics_by_breakdown_and_ch_flag_df %>%
  mutate(
    SDC = ifelse(TOTAL_PATIENTS %in% c(1, 2, 3, 4), 1, 0),
    SDC_ITEMS_PER_PATIENT_MONTH =
      ifelse(
        test = SDC == 1, 
        yes = NA_integer_, 
        no = janitor::round_half_up(ITEMS_PER_PATIENT_MONTH)
      ),
    SDC_COST_PER_PATIENT_MONTH =
      ifelse(
        test = SDC == 1L, 
        yes = NA_integer_, 
        no = janitor::round_half_up(COST_PER_PATIENT_MONTH)
      ),
    SDC = ifelse(TOTAL_PATIENTS_UNIQUE_MEDICINES %in% c(1, 2, 3, 4), 1, 0),
    SDC_UNIQUE_MEDICINES_PER_PATIENT_MONTH =
      ifelse(
        test = SDC == 1, 
        yes = NA_integer_, 
        no = janitor::round_half_up(UNIQUE_MEDICINES_PER_PATIENT_MONTH, 1)
      ),
    SDC = ifelse(TOTAL_PATIENTS_TEN_OR_MORE %in% c(1, 2, 3, 4), 1, 0),
    SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH =
      ifelse(
        test = SDC == 1, 
        yes = NA_integer_, 
        no = janitor::round_half_up(PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH)
      )
  ) %>%
  select(-SDC)

# Format for highcharter
metrics_by_breakdown_and_ch_flag_df <- metrics_by_breakdown_and_ch_flag_df %>%
  careHomePrescribingScrollytellR::format_data_raw("CH_FLAG")

# Add to data-raw/
usethis::use_data(metrics_by_breakdown_and_ch_flag_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)
