library(magrittr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the care home FACT table
fact_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM DALL_REF.INT615_ITEM_LEVEL_BASE")
)

# Create gender flag per NHS_NO
gender_flag <- fact_db %>%
  dplyr::group_by(NHS_NO) %>%
  dplyr::mutate(
    MALE_FLAG = dplyr::case_when(PDS_GENDER == 1 ~ 1, TRUE ~ 0),
    FEMALE_FLAG = dplyr::case_when(PDS_GENDER == 2 ~ 1, TRUE ~ 0),
    UNKNOWN_FLAG = dplyr::case_when(PDS_GENDER %in% c(0, 9) ~ 1, TRUE ~ 0)
  )

# Aggregate by NHS number
gender_flag_patient <- gender_flag %>%
  dplyr::group_by(NHS_NO) %>%
  dplyr::summarise(
    MALE_FLAG = dplyr::sum(MALE_FLAG),
    FEMALE_FLAG = dplyr::sum(FEMALE_FLAG),
    UNKNOWN_FLAG = dplyr::sum(UNKNOWN_FLAG)
  )

# Apply logic for each NHS_NO
# Male & Female = Unknown
# Male & Unknown = Male
# Female & Unknown = Female
gender_flag_db <- gender_flag_patient %>%
  dplyr::group_by(NHS_NO) %>%
  dplyr::mutate(
    GENDER_FLAG = dplyr::case_when(
      MALE_FLAG > 0 & FEMALE_FLAG > 0 ~ "UNKNOWN",
      MALE_FLAG > 0 & UNKNOWN_FLAG > 0 ~ "MALE",
      FEMALE_FLAG > 0 & UNKNOWN_FLAG > 0 ~ "FEMALE",
      MALE_FLAG > 0 ~ "MALE",
      FEMALE_FLAG > 0 ~ "FEMALE",
      TRUE ~ "UNKNOWN"
    )
  )

# add single gender per patient to fact table
fact_db <- fact_db %>%
  dplyr::left_join(
    y = gender_flag_db %>%
      dplyr::select(
        NHS_NO, GENDER_FLAG
      ),
    by = c(
      "NHS_NO" = "NHS_NO"
    )
  )

# Assign max age to each NHS NO
age_flag_db <- fact_db %>%
  dplyr::group_by(NHS_NO) %>%
  dplyr::summarise(
    MAX_AGE = max(CALC_AGE)
  )

# add single age per patient to fact table
fact_db <- fact_db %>%
  dplyr::left_join(
    y = age_flag_db %>%
      dplyr::select(
        NHS_NO, MAX_AGE
      ),
    by = c(
      "NHS_NO" = "NHS_NO"
    )
  )

# Aggregate number of patients per month by CH_FLAG
ch_agg_ym_df <- fact_db %>%
  dplyr::mutate(
    CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home")
  ) %>%
  dplyr::group_by(CH_FLAG, YEAR_MONTH) %>%
  dplyr::summarise(
    TOTAL_PATIENTS = dplyr::n_distinct(NHS_NO),
  ) %>%
  dplyr::ungroup()

# Add overall mean and format for highcharter
patient_count_distinct_df <- ch_agg_ym_df %>%
  dplyr::union_all(
    y = ch_agg_ym_df %>%
      dplyr::group_by(CH_FLAG) %>%
      dplyr::summarise(TOTAL_PATIENTS = mean(TOTAL_PATIENTS))
  ) %>%
  dplyr::arrange(YEAR_MONTH) %>%
  dplyr::collect() %>%
  dplyr::mutate(
    YEAR_MONTH = lubridate::ym(YEAR_MONTH),
    CH_FLAG = forcats::fct_rev(CH_FLAG)
  )

# Add to data-raw/
usethis::use_data(patient_count_distinct_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)
