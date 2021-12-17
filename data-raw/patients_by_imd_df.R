library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the geography lookup table (Region, STP and LA)
postcode_db <- con %>%
  tbl(from = "INT615_POSTCODE_LOOKUP")

# Create a lazy table from the care home FACT table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Join postcode_db and fact_db on postcode
fact_db <- fact_db %>%
  left_join(
    y = postcode_db,
    by = c("PCD_NO_SPACES" = "POSTCODE")
  )

# Count care home patients in each quintile
patients_by_imd_db <- fact_db %>%
  group_by(IMD_QUINTILE) %>%
  filter(CH_FLAG == 1) %>%
  ungroup() %>%
  group_by(IMD_QUINTILE) %>%
  summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
  ungroup()

# Add percentage
patients_by_imd_db <- patients_by_imd_db %>%
  mutate(PCT_PATIENTS = TOTAL_PATIENTS / sum(TOTAL_PATIENTS) * 100)

# Collect and apply SDC to total patients and percentage of patients
patients_by_imd_df <- patients_by_imd_db %>%
  arrange(IMD_QUINTILE) %>%
  collect() %>%
  mutate(
    SDC = ifelse(TOTAL_PATIENTS %in% c(1, 2, 3, 4), 1, 0),
    SDC_TOTAL_PATIENTS = 
      ifelse(SDC == 1, NA_integer_, round(TOTAL_PATIENTS, -1)),
    SDC_PCT_PATIENTS =
      ifelse(SDC == 1, NA_integer_, janitor::round_half_up(PCT_PATIENTS))
  ) %>% 
  select(-c(SDC, TOTAL_PATIENTS, PCT_PATIENTS))

# Add to data-raw/
usethis::use_data(patients_by_imd_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)
