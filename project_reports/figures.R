library(dplyr)
library(dbplyr)
devtools::load_all()

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the item level base table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# 286k monthly average
careHomePrescribingScrollytellR::patients_by_prescribing_status_df %>%
  ungroup() %>%
  filter(PRESCRIBING_STATUS == "Received care home prescribing") %>%
  summarise(mean(SDC_TOTAL_PATIENTS))

# Care home patients: 35M items (4%) for £324M (7%) from 472k patients (5%) 
fact_db %>%
  group_by(CH_FLAG) %>%
  summarise(
    TOTAL_ITEMS = sum(ITEM_COUNT),
    TOTAL_COST = sum(ITEM_PAY_DR_NIC * 0.01),
    TOTAL_PATIENTS = n_distinct(NHS_NO)
  ) %>%
  ungroup() %>%
  mutate(
    PCT_ITEMS = TOTAL_ITEMS / sum(TOTAL_ITEMS),
    PCT_COST = TOTAL_COST / sum(TOTAL_COST),
    PCT_PATIENTS = TOTAL_PATIENTS / sum(TOTAL_PATIENTS)
  )

# How many months did a patient receive a prescription in a care home
fact_db %>%
  filter(CH_FLAG == "Care home") %>%
  group_by(NHS_NO) %>%
  summarise(TOTAL_MONTHS = n_distinct(YEAR_MONTH)) %>%
  ungroup() %>%
  count(TOTAL_MONTHS) %>%
  mutate(PCT = n / sum(n))

# 66% female
careHomePrescribingScrollytellR::metrics_by_breakdown_and_ch_flag_df %>%
  filter(BREAKDOWN == "Demographical - Gender") %>%
  filter(CH_FLAG == "Care home") %>%
  select(SUB_BREAKDOWN_NAME, TOTAL_PATIENTS) %>%
  mutate(PCT = TOTAL_PATIENTS / sum(TOTAL_PATIENTS) * 100)

# 59% 85+            
careHomePrescribingScrollytellR::metrics_by_breakdown_and_ch_flag_df %>%
  filter(BREAKDOWN == "Demographical - Age Band") %>%
  filter(CH_FLAG == "Care home") %>%
  select(SUB_BREAKDOWN_NAME, TOTAL_PATIENTS) %>%
  mutate(PCT = TOTAL_PATIENTS / sum(TOTAL_PATIENTS) * 100)

# Nursing / residential (41% residential, 45% nursing, 3% both, 11% unknown)
careHomePrescribingScrollytellR::metrics_by_breakdown_and_ch_flag_df %>%
  filter(BREAKDOWN == "Additional - Care home type") %>%
  filter(CH_FLAG == "Care home") %>%
  select(NURSING_HOME_FLAG, RESIDENTIAL_HOME_FLAG, TOTAL_PATIENTS) %>%
  mutate(PCT = TOTAL_PATIENTS / sum(TOTAL_PATIENTS) * 100)
  