# Library
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Results item table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Create a lazy table from the year month table
cip_db <- con %>%
  tbl(from = in_schema("DIM", "CIP_PATIENT_DIM"))

# Create a lazy table from the year month table
year_month_db <- con %>%
  tbl(from = in_schema("DALL_REF", "YEAR_MONTH_DIM"))

# Filtered year month
year_month_db <- year_month_db %>% 
  filter(FINANCIAL_YEAR == "2020/2021") %>%
  select(YEAR_MONTH) %>% 
  mutate(TMP = 1)

# Process fact table 
ch_pats <- fact_db %>% 
  filter(CH_FLAG == 1) %>% 
  select(NHS_NO) %>% 
  distinct() %>% 
  mutate(TMP = 1)

# Process Year month table prior to join
cross_pat <- ch_pats %>% 
  full_join(year_month_db) %>% 
  select(-TMP)

# Results item table
# Max CH_FLAG per patient per month
items <- fact_db %>% 
  select(YEAR_MONTH, NHS_NO, CH_FLAG) %>% 
  inner_join(ch_pats %>% select(-TMP)) %>% 
  distinct() %>% 
  group_by(YEAR_MONTH, NHS_NO) %>% 
  summarise(CH_FLAG = max(CH_FLAG)) %>% 
  ungroup()

# Long table
cross <- cross_pat %>% 
  left_join(items) %>% 
  arrange(NHS_NO, YEAR_MONTH)

# Deceased patients
dead <- cip_db %>% 
  filter(DOD < '01-APR-21') %>% 
  select(NHS_NO_CIP, DOD) %>% 
  inner_join(ch_pats %>% select(-TMP), by = c("NHS_NO_CIP" = "NHS_NO")) %>%
  mutate(
    YEAR_MONTH = TO_CHAR(DOD, 'YYYYMM'),
    DOD_MONTH = 1
  ) %>% 
  select(-DOD)

# Combine and process
total <- cross %>% 
  left_join(
    y = dead %>% mutate(YEAR_MONTH = as.integer(YEAR_MONTH)),
    by = c("YEAR_MONTH", "NHS_NO" = "NHS_NO_CIP")
  ) %>% 
  group_by(NHS_NO) %>% 
  mutate(
    DOD_MONTH = ifelse(is.na(DOD_MONTH), 0, 1),
    DOD_MONTH = cumsum(DOD_MONTH)
  ) %>% 
  ungroup() %>% 
  mutate(
    CH_FLAG = as.character(CH_FLAG),
    CH_FLAG = ifelse(DOD_MONTH == '1', "Deceased", CH_FLAG),
    CH_FLAG = ifelse(is.na(CH_FLAG), "Received no prescribing", CH_FLAG),
    CH_FLAG = ifelse(CH_FLAG == '1', "Received care home prescribing", CH_FLAG),
    CH_FLAG = ifelse(
      CH_FLAG == '0', "Received non-care home prescribing", CH_FLAG
      )
    )

# DF for 1st plot
area <- total %>% 
  group_by(YEAR_MONTH) %>% 
  count(CH_FLAG) %>% 
  ungroup() %>% 
  collect() %>% 
  mutate(
    n = round(n/1000),
    YEAR = substr(YEAR_MONTH, 1, 4),
    MONTH = month.abb[as.integer(substr(YEAR_MONTH, 5, 6))],
    MONTH_YEAR = paste0(MONTH, " - ", YEAR),
    CH_FLAG = factor(CH_FLAG, levels = c(
      "Received non-care home prescribing",
      "Received care home prescribing",
      "Deceased",
      "Received no prescribing")
    )
  ) %>% 
  select(-c(YEAR, MONTH)) %>% 
  arrange(YEAR_MONTH)

# First Series
monthly_prescribing_status_with_prescribing_df <- area %>% 
  filter(
    CH_FLAG == "Received non-care home prescribing" |
      CH_FLAG == "Received care home prescribing"
  )

# Second Series 
monthly_prescribing_status_without_prescribing_df <- area %>% 
  filter(CH_FLAG == "Received no prescribing" | CH_FLAG == "Deceased")

# Add to data
usethis::use_data(
  monthly_prescribing_status_with_prescribing_df,
  overwrite = TRUE
)

# Add to data
usethis::use_data(
  monthly_prescribing_status_without_prescribing_df,
  overwrite = TRUE
)

# Disconnect from database
DBI::dbDisconnect(con)