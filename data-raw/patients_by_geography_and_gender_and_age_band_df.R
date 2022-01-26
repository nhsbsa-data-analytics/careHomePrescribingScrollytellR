library(dplyr)
library(dbplyr)
devtools::load_all()

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the item level base table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Filter to care home only and add a dummy overall column
fact_db <- fact_db %>%
  filter(CH_FLAG == "Care home") %>%
  mutate(OVERALL = "Overall") # dummy col

# Loop over each geography and aggregate
for (geography_name in names(careHomePrescribingScrollytellR::geographys)) {

  # Extract the geography cols
  geography_cols <-
    careHomePrescribingScrollytellR::geographys[[geography_name]] %>%
    purrr::set_names(
      nm = stringr::str_replace(
        string = names(.),
        pattern = "BREAKDOWN",
        replacement = "GEOGRAPHY"
      )
    )

  # Overall total patients
  tmp_db <- fact_db %>%
    group_by(
      GEOGRAPHY = geography_name,
      across(all_of(unname(geography_cols))),
      GENDER,
      AGE_BAND
    ) %>%
    rename(!!!geography_cols) %>%
    # If the SUB_GEOGRAPHY_NAME is NA then set gender to NA
    mutate(
      GENDER = ifelse(is.na(SUB_GEOGRAPHY_NAME), NA, GENDER),
      # NA if SUB_GEOGRAPHY_NAME is NA or if GENDER is NA
      AGE_BAND = ifelse(
        test = is.na(SUB_GEOGRAPHY_NAME) | is.na(GENDER),
        yes = NA,
        AGE_BAND
      )
    ) %>%
    summarise(TOTAL_PATIENTS = n_distinct(NHS_NO))

  # Calculate the percentage
  tmp_db <- tmp_db %>%
    ungroup(GENDER, AGE_BAND, TOTAL_PATIENTS) %>%
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
  # NOTE: Don't know why but collect won't work without this... throws error:
  # ORA-24374: define not done before fetch or execute and fetch
  # but works this filter (which actually filters nothing...)
  filter(!is.na(GEOGRAPHY)) %>%
  collect()

# Get all the possible combinations
patients_by_geography_and_gender_and_age_band_df <-
  patients_by_geography_and_gender_and_age_band_df %>%
  tidyr::complete(
    # Only geographies that already exist
    tidyr::nesting(GEOGRAPHY, SUB_GEOGRAPHY_CODE, SUB_GEOGRAPHY_NAME),
    # Only age band and gender combinations that exist (so we only have NA age
    # band for NA genders)
    tidyr::nesting(AGE_BAND, GENDER),
    fill = list(
      TOTAL_PATIENTS = 0L,
      PCT_PATIENTS = 0
    )
  )

# Apply SDC to total patients and percentage of patients
patients_by_geography_and_gender_and_age_band_df <-
  patients_by_geography_and_gender_and_age_band_df %>%
  mutate(
    SDC = ifelse(TOTAL_PATIENTS %in% c(1, 2, 3, 4), 1, 0),
    SDC_TOTAL_PATIENTS =
      ifelse(SDC == 1, NA_integer_, round(TOTAL_PATIENTS, -1)),
    SDC_PCT_PATIENTS =
      ifelse(SDC == 1, NA_integer_, janitor::round_half_up(PCT_PATIENTS, 1))
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
