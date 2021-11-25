# Load library
library(magrittr)
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the care home FACT table
fact_db <- tbl(
  src = con,
  from = sql("SELECT * FROM DALL_REF.INT615_ITEM_LEVEL_BASE")
)

# Filter to care home only and add gender and age band groups to the FACT table
fact_db <- fact_db %>%
  filter(CH_FLAG == 1) %>%
  mutate(
    PDS_GENDER = case_when(
      PDS_GENDER == 1 ~ "Male",
      PDS_GENDER == 2 ~ "Female",
      PDS_GENDER %in% c(0, 9) ~ "Unknown",
      TRUE ~ "Error"
    ),
    AGE_BAND = case_when(
      CALC_AGE < 70 ~ "65-69",
      CALC_AGE < 75 ~ "70-74",
      CALC_AGE < 80 ~ "75-79",
      CALC_AGE < 85 ~ "80-84",
      CALC_AGE < 90 ~ "85-89",
      TRUE ~ "90+"
    )
  )

# Join with geography (STP, LA)

geography_lookup <- tbl(
  src = con,
  from = sql("SELECT * FROM KAYGO.INT615_PCD_REF")
)

geography_lookup <- geography_lookup %>%
  select(POSTCODE, PCD_STP_NAME, PCD_LAD_NAME, PCD_REGION_NAME) # keep only three geography of interest for the age/gender breakdown


fact_db <- fact_db %>%
  left_join(
    y = geography_lookup,
    by = c("PCD_NO_SPACES" = "POSTCODE")
  )


# Create various aggregation - Overall, By year month, By STP and year month, By LA and year month



# Total patients by gender and age band
patients_by_gender_and_age_band_df <-
  union_all(
    # Overall
    x = fact_db %>%
      group_by(PDS_GENDER, AGE_BAND) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup(),
    # By year month
    y = fact_db %>%
      group_by(YEAR_MONTH, PDS_GENDER, AGE_BAND) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup()
  ) %>%
  mutate(
    YEAR_MONTH = ifelse(is.na(YEAR_MONTH), "Overall", as.character(YEAR_MONTH))
  ) %>%
  relocate(YEAR_MONTH) %>%
  arrange(YEAR_MONTH, PDS_GENDER, AGE_BAND) %>%
  collect() %>%
  # Format for highcharter
  mutate(
    YEAR_MONTH = forcats::fct_relevel(YEAR_MONTH, "Overall"),
    GEOGRAPHY = "Overall",
    LEVEL = "Overall"
  ) %>%
  arrange(YEAR_MONTH)

# Process for STP (union all didn't work so bit messy)

patients_by_gender_and_age_band_stp_df <-
  union_all(
    # Overall
    x = fact_db %>%
      group_by(PDS_GENDER, AGE_BAND, PCD_STP_NAME) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup(),
    # By year month
    y = fact_db %>%
      group_by(YEAR_MONTH, PDS_GENDER, AGE_BAND, PCD_STP_NAME) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup()
  ) %>%
  mutate(
    YEAR_MONTH = ifelse(is.na(YEAR_MONTH), "Overall", as.character(YEAR_MONTH))
  ) %>%
  relocate(YEAR_MONTH) %>%
  arrange(YEAR_MONTH, PDS_GENDER, AGE_BAND) %>%
  collect() %>%
  # Format for highcharter
  mutate(
    YEAR_MONTH = forcats::fct_relevel(YEAR_MONTH, "Overall"),
    LEVEL = "STP"
  ) %>%
  rename(GEOGRAPHY = PCD_STP_NAME) %>%
  arrange(YEAR_MONTH)

# Process for LA

patients_by_gender_and_age_band_la_df <-
  union_all(
    # Overall
    x = fact_db %>%
      group_by(PDS_GENDER, AGE_BAND, PCD_LAD_NAME) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup(),
    # By year month
    y = fact_db %>%
      group_by(YEAR_MONTH, PDS_GENDER, AGE_BAND, PCD_LAD_NAME) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup()
  ) %>%
  mutate(
    YEAR_MONTH = ifelse(is.na(YEAR_MONTH), "Overall", as.character(YEAR_MONTH))
  ) %>%
  relocate(YEAR_MONTH) %>%
  arrange(YEAR_MONTH, PDS_GENDER, AGE_BAND) %>%
  collect() %>%
  # Format for highcharter
  mutate(
    YEAR_MONTH = forcats::fct_relevel(YEAR_MONTH, "Overall"),
    LEVEL = "LA"
  ) %>%
  rename(GEOGRAPHY = PCD_LAD_NAME) %>%
  arrange(YEAR_MONTH)


# Process for Region
# 48,608 records could not match with region

patients_by_gender_and_age_band_region_df <-
  union_all(
    # Overall
    x = fact_db %>%
      group_by(PDS_GENDER, AGE_BAND, PCD_REGION_NAME) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup(),
    # By year month
    y = fact_db %>%
      group_by(YEAR_MONTH, PDS_GENDER, AGE_BAND, PCD_REGION_NAME) %>%
      summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
      ungroup()
  ) %>%
  mutate(
    YEAR_MONTH = ifelse(is.na(YEAR_MONTH), "Overall", as.character(YEAR_MONTH)),
    PCD_REGION_NAME = ifelse(is.na(PCD_REGION_NAME), "INVALID POSTCODE", as.character(PCD_REGION_NAME))
  ) %>%
  relocate(YEAR_MONTH) %>%
  arrange(YEAR_MONTH, PDS_GENDER, AGE_BAND) %>%
  collect() %>%
  # Format for highcharter
  mutate(
    YEAR_MONTH = forcats::fct_relevel(YEAR_MONTH, "Overall"),
    LEVEL = "Region"
  ) %>%
  rename(GEOGRAPHY = PCD_REGION_NAME) %>%
  arrange(YEAR_MONTH)

# rbind

patients_by_gender_and_age_band_df <- bind_rows(
  patients_by_gender_and_age_band_df,
  patients_by_gender_and_age_band_stp_df,
  patients_by_gender_and_age_band_la_df,
  patients_by_gender_and_age_band_region_df
)

# Also keep geography_lookup for the input select

stp_la_region_lookup <- geography_lookup %>%
  distinct(PCD_STP_NAME, PCD_LAD_NAME, PCD_REGION_NAME) %>%
  collect() %>%
  rename(
    STP = PCD_STP_NAME,
    LA = PCD_LAD_NAME,
    Region = PCD_REGION_NAME
  ) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "GEOGRAPHY", values_to = "NAME") %>%
  arrange(GEOGRAPHY, NAME) %>%
  tibble::add_row(GEOGRAPHY = "Overall", NAME = "Overall") %>%
  distinct(GEOGRAPHY, NAME)





# Add to data-raw/
usethis::use_data(patients_by_gender_and_age_band_df, overwrite = TRUE)
usethis::use_data(stp_la_region_lookup, overwrite = TRUE)





# Disconnect from database
DBI::dbDisconnect(con)
