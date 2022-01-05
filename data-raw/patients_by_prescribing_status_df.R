library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the care home FACT table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Create a lazy table from the CIP patient dim table
cip_db <- con %>%
  tbl(from = in_schema("DIM", "CIP_PATIENT_DIM"))

# Limit FACT table to NHS numbers with at least some care home prescribing
care_home_patient_fact_db <- fact_db %>%
  semi_join(
    y = fact_db %>% 
      filter(CH_FLAG == 1L) %>% 
      select(NHS_NO),
    copy = TRUE
  )

# Get the max care home flag for each patient in each month
patients_by_prescribing_status_db <- care_home_patient_fact_db %>%
  group_by(YEAR_MONTH, NHS_NO) %>% 
  summarise(CH_FLAG = max(CH_FLAG)) %>%
  ungroup()

# Get a row for each patient in each month
patients_by_prescribing_status_db <- patients_by_prescribing_status_db %>%
  tidyr::complete(YEAR_MONTH, NHS_NO)

# Indicate if patient is deceased
patients_by_prescribing_status_db <- patients_by_prescribing_status_db %>%
  left_join(
    y = cip_db %>% 
      mutate(
        YMOD = as.integer(TO_CHAR(DOD, "YYYYMM")),
        DECEASED = 1L # Join where YEAR_MONTH after YMOD
      ) %>%
      select(NHS_NO = NHS_NO_CIP, YMOD, DECEASED),
    sql_on = "LHS.NHS_NO = RHS.NHS_NO AND LHS.YEAR_MONTH > RHS.YMOD",
    suffix = c("", ".y"),
    copy = TRUE
  ) %>% 
  select(-NHS_NO.y)

# Categorise the patient in each year month
patients_by_prescribing_status_db <- patients_by_prescribing_status_db %>%
  mutate(
    PRESCRIBING_STATUS = case_when(
      DECEASED == 1L ~ "Deceased",
      CH_FLAG == 1L ~ "Received care home prescribing",
      CH_FLAG == 0L ~ "Received non-care home prescribing",
      is.na(CH_FLAG) ~ "Recieved no prescribing",
      TRUE ~ "Error"
    )
  )

# Aggregate to year month
patients_by_prescribing_status_db <- patients_by_prescribing_status_db %>% 
  count(YEAR_MONTH, PRESCRIBING_STATUS, name = "TOTAL_PATIENTS") %>%
  arrange(YEAR_MONTH, PRESCRIBING_STATUS)

# Collect and format for highcharter
patients_by_prescribing_status_df <-
  patients_by_prescribing_status_db %>%
  collect() %>%
  # Move deceased to last factor
  mutate(
    PRESCRIBING_STATUS = forcats::fct_relevel(
      PRESCRIBING_STATUS, 
      "Deceased", 
      after = Inf
    )
  )

# Add to data
usethis::use_data(patients_by_prescribing_status_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)