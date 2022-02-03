# Library
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Results item table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Results item table
ab_db <- con %>%
  tbl(from = in_schema("DALL_REF", "ADDRESSBASE_PLUS"))

# Postcodes with a CH to enable later aggregations
ch_pcd <- ab_db %>% 
  filter(
    COUNTRY == "E",
    RELEASE_DATE == "15-MAR-21",
    CLASS == 'RI01'
  ) %>% 
  select(POSTCODE = POSTCODE_LOCATOR) %>% 
  distinct() %>% 
  mutate(
    POSTCODE = REPLACE(POSTCODE, " ", ""),
    CH_PCD = 1
  )

# Workflow Part 2.2. Electronic Prescriptions - 89% for 65+ age group
fact_db %>% 
  group_by(EPS_FLAG) %>% 
  summarise(FORM_COUNT = n_distinct(PF_ID)) %>% 
  ungroup() %>% 
  mutate(
    TOTAL = sum(FORM_COUNT),
    PROP = FORM_COUNT / TOTAL
  )

# Workflow 2.4. Total Number of 65+ Forms - 257M Forms
fact_db %>% 
  summarise(FORM_COUNT = n_distinct(PF_ID))

# Workflow Part 2.6. Total Address Record IDs - 8M Address Records
fact_db %>% 
  summarise(ADDRESS_COUNT = n_distinct(SINGLE_LINE_ADDRESS))

# Workflow Part 3.1. Total AB Addresses for Used Release Date - 32M
ab_db %>% 
  filter(
    COUNTRY == "E",
    RELEASE_DATE == "15-MAR-21"
  ) %>% 
  tally()

# Workflow Part 4.2. Proportion of CH-Address CH Exact Matches - 9%
fact_db %>% 
  filter(CH_FLAG == "Care home") %>% 
  group_by(MATCH_TYPE) %>% 
  summarise(ADDRESS_COUNT = n_distinct(SINGLE_LINE_ADDRESS)) %>% 
  ungroup() %>% 
  mutate(
    TOTAL = sum(ADDRESS_COUNT),
    PROP = ADDRESS_COUNT / TOTAL
  )

# Workflow 4.9. CH_FLAG by Address Record Id Count - 96.7% vs 3.3%
fact_db %>% 
  left_join(ch_pcd) %>% 
  group_by(CH_PCD) %>% 
  summarise(ADDRESS_COUNT = n_distinct(SINGLE_LINE_ADDRESS)) %>% 
  ungroup() %>% 
  mutate(
    TOTAL = sum(ADDRESS_COUNT),
    PROP = ADDRESS_COUNT / TOTAL
  )

# Workflow 5.6. Hierarchy of Matches - Postcode with CH
fact_db %>% 
  inner_join(ch_pcd) %>% 
  group_by(MATCH_TYPE) %>% 
  summarise(ADDRESS_COUNT = n_distinct(SINGLE_LINE_ADDRESS)) %>% 
  ungroup() %>% 
  mutate(
    TOTAL = sum(ADDRESS_COUNT),
    PROP = ADDRESS_COUNT / TOTAL
  )

# Workflow 5.6. Hierarchy of Matches - Postcode without CH
fact_db %>% 
  anti_join(ch_pcd) %>% 
  mutate(MATCH_TYPE = ifelse(MATCH_TYPE == "KEY WORD", "KEY WORD", "NO MATCH")) %>% 
  group_by(MATCH_TYPE) %>% 
  summarise(ADDRESS_COUNT = n_distinct(SINGLE_LINE_ADDRESS)) %>% 
  ungroup() %>% 
  mutate(
    TOTAL = sum(ADDRESS_COUNT),
    PROP = ADDRESS_COUNT / TOTAL
  )

# Workflow 6.3. Results Interpretation - Sankey Diagram Figures
apr_nhs_no <- fact_db %>% 
  filter(
    CH_FLAG == "Care home",
    YEAR_MONTH == 202004
  ) %>% 
  select(NHS_NO) %>% 
  distinct()

# Workflow 6.3. April Distinct CH NHS_NO Count
apr_nhs_no %>% 
  tally()

# Workflow 6.3. Prescription Status of Above NHS_NO in May 2020
fact_db %>% 
  filter(YEAR_MONTH == 202005) %>% 
  select(NHS_NO, CH_FLAG) %>% 
  inner_join(apr_nhs_no) %>% 
  group_by(CH_FLAG) %>% 
  summarise(PAT_COUNT = n_distinct(NHS_NO))

# Workflow 6.5. Number of Patient Count Address Records: 483 Records
fact_db %>% 
  filter(
    CH_FLAG == "Care home",
    MATCH_TYPE == "PATIENT COUNT"
  ) %>% 
  summarise(n_distinct(SINGLE_LINE_ADDRESS))

# Workflow Part 6.5. Keyword Type 
fact_db %>% 
  filter(
    CH_FLAG == "Care home",
    MATCH_TYPE == "KEY WORD"
  ) %>% 
  select(SINGLE_LINE_ADDRESS, PF_ID) %>% 
  mutate(
    KW = case_when(
      REGEXP_LIKE(SINGLE_LINE_ADDRESS, "CARE HOME|CARE-HOME") ~ "CH",
      REGEXP_LIKE(SINGLE_LINE_ADDRESS, "NURSING HOME|NURSING-HOME") ~ "NH",
      REGEXP_LIKE(SINGLE_LINE_ADDRESS, "RESIDENTIAL HOME|RESIDENTIAL-HOME") ~ "RH",
      REGEXP_LIKE(SINGLE_LINE_ADDRESS, "REST HOME|REST-HOME") ~ "RT"
    )
  ) %>% 
  group_by(KW) %>% 
  summarise(FORM_COUNT = n_distinct(PF_ID)) %>% 
  ungroup() %>% 
  mutate(
    TOTAL = sum(FORM_COUNT),
    PROP = FORM_COUNT / TOTAL
  )

# Workflow Part 6.6. final Form Counts for Overall Accuracy
fact_db %>% 
  filter(CH_FLAG == "Care home") %>% 
  group_by(MATCH_TYPE) %>% 
  summarise(FORM_COUNT = n_distinct(PF_ID)) %>% 
  ungroup() %>% 
  mutate(
    TOTAL = sum(FORM_COUNT),
    PROP = FORM_COUNT / TOTAL
  )

# Disconnect
DBI::dbDisconnect(con)