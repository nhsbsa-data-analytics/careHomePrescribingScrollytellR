# Library
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

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

# Filter to BNF chapters 1 - 4 and 6 - 10 (inline with polypharmacy) in
# 2020/2021 and subset columns
drug_db <- drug_db %>%
  filter(BNF_CHAPTER %in% c(01, 02, 03, 04, 06, 07, 08, 09, 10)) %>%
  inner_join(year_month_db) %>%
  select(YEAR_MONTH, RECORD_ID, CHEMICAL_SUBSTANCE_BNF_DESCR)

# Item level fact table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Get a single gender and age for the period
patient_db <- fact_db %>%
  group_by(NHS_NO) %>%
  summarise(
    # Gender
    MALE_COUNT = sum(ifelse(PDS_GENDER == 1, 1, 0)),
    FEMALE_COUNT = sum(ifelse(PDS_GENDER == 2, 1, 0)),
    # Take the max age
    AGE = max(CALC_AGE)
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
      TRUE ~ "90+")
    )

# Join fact data to patient level dimension
fact_db <- fact_db %>%
  left_join(y = patient_db, by = "NHS_NO")

# Cost Metrics
cost_by_age_gender_and_ch_flag_df <- fact_db %>%
  filter(GENDER == "Male" | GENDER == "Female") %>% 
  select(NHS_NO, YEAR_MONTH, ITEM_PAY_DR_NIC, GENDER, AGE_BAND, CH_FLAG) %>%
  mutate(CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home")) %>%
  # Year month level
  group_by(YEAR_MONTH, GENDER, AGE_BAND, CH_FLAG) %>%
  summarise(
    TOTAL_COST = sum(ITEM_PAY_DR_NIC * 0.01),
    TOTAL_PATIENTS = n_distinct(NHS_NO),
    COST_PER_PATIENT = sum(ITEM_PAY_DR_NIC * 0.01) / n_distinct(NHS_NO)
  ) %>%
  ungroup() %>% 
  # Metric level
  group_by(GENDER, AGE_BAND, CH_FLAG) %>% 
  summarise(COST_PER_PATIENT = round(mean(COST_PER_PATIENT), 2)) %>% 
  ungroup() %>% 
  # Collect and change factor-levels
  collect() %>% 
  mutate(AGE_BAND = factor(
    AGE_BAND, levels = c("65-69","70-74","75-79","80-84","85-89", "90+"))
  ) %>% 
  arrange(AGE_BAND) %>% 
  mutate(METRIC = paste0(GENDER, ' - ', CH_FLAG))

# Add to data-raw/
usethis::use_data(
  cost_by_age_gender_and_ch_flag_df,
  overwrite = TRUE
)

# Disconnect
DBI::dbDisconnect(con)