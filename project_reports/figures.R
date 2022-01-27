library(dplyr)
library(dbplyr)
devtools::load_all()

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the item level base table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# 01_intro

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
