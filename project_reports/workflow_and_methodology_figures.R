# Library
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Results item table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Address matches table
address_matched_db <- con %>%
  tbl(from = in_schema("ADAIV", "INT615_ADDRESS_MATCHED"))

# Create a lazy table from the geography lookup table (Region, STP and LA)
postcode_db <- con %>%
  tbl(from = in_schema("ADAIV", "INT615_POSTCODE_LOOKUP")) %>%
  select(POSTCODE)

# CQC table
cqc_db <- con %>%
  tbl(from = in_schema("ADAIV", "INT615_CQC"))

# AddressBase Plus table
ab_db <- con %>%
  tbl(from = in_schema("DALL_REF", "ADDRESSBASE_PLUS"))

# Filter AddressBase Plus to English properties at the end of 2021 FY
ab_db <- ab_db %>% 
  filter(
    COUNTRY == "E",
    substr(CLASS, 1, 1) != "L", # Land
    substr(CLASS, 1, 1) != "O", # Other (Ordnance Survey only)
    substr(CLASS, 1, 2) != "PS", # Street Record
    substr(CLASS, 1, 2) != "RC", # Car Park Space
    substr(CLASS, 1, 2) != "RG", # Lock-Up / Garage / Garage Court
    substr(CLASS, 1, 1) != "Z", # Object of interest
    RELEASE_DATE == TO_DATE("2021-03-15", "YYYY-MM-DD")
  ) %>%
  # Take POSTCODE_LOCATOR as the postcode as it is equal to POSTCODE (whenever
  # one exists) but more complete and tidy it
  mutate(POSTCODE = POSTCODE_LOCATOR) %>%
  addressMatchR::tidy_postcode(col = POSTCODE)

# Postcodes with a CH to enable later aggregations
ch_pcd <- ab_db %>% 
  filter(CLASS == 'RI01') %>% 
  distinct(POSTCODE) %>% 
  mutate(CH_PCD = 1L)

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

# CQC vs AB matches

# CQC

# Convert registration and deregistration columns to dates and filter to 2020/21
cqc_db <- cqc_db %>%
  mutate(
    REGISTRATION_DATE = ifelse(
      test = is.na(REGISTRATION_DATE),
      yes = NA,
      no = TO_DATE(REGISTRATION_DATE, "YYYY-MM-DD")
    ),
    DEREGISTRATION_DATE = ifelse(
      test = is.na(DEREGISTRATION_DATE),
      yes = NA,
      no = TO_DATE(DEREGISTRATION_DATE, "YYYY-MM-DD")
    )
  ) %>%
  filter(
    REGISTRATION_DATE <= TO_DATE("2021-03-31", "YYYY-MM-DD"),
    is.na(DEREGISTRATION_DATE) |
      DEREGISTRATION_DATE >= TO_DATE("2020-04-01", "YYYY-MM-DD")
  )

# Create a tidy distinct single line address and postcode
cqc_db <- cqc_db %>%
  mutate(
    SINGLE_LINE_ADDRESS = paste(
      NAME,
      POSTAL_ADDRESS_LINE_1,
      POSTAL_ADDRESS_LINE_2,
      POSTAL_ADDRESS_TOWN_CITY,
      POSTAL_ADDRESS_COUNTY
    )
  ) %>%
  addressMatchR::tidy_single_line_address(col = SINGLE_LINE_ADDRESS) %>%
  addressMatchR::tidy_postcode(col = POSTAL_CODE) %>%
  distinct(
    POSTCODE = POSTAL_CODE, 
    SINGLE_LINE_ADDRESS_LOOKUP = SINGLE_LINE_ADDRESS
  )

# AB

# Create and tidy the DPA and GEO single line addresses
ab_db <- ab_db %>%
  addressMatchR::calc_addressbase_plus_dpa_single_line_address() %>%
  addressMatchR::calc_addressbase_plus_geo_single_line_address() %>%
  addressMatchR::tidy_single_line_address(col = DPA_SINGLE_LINE_ADDRESS) %>%
  addressMatchR::tidy_single_line_address(col = GEO_SINGLE_LINE_ADDRESS) %>%
  select(
    UPRN,
    POSTCODE,
    DPA_SINGLE_LINE_ADDRESS,
    GEO_SINGLE_LINE_ADDRESS
  )

# When DPA != GEO then add a CORE single line address
ab_db <-
  union_all(
    x = ab_db %>%
      filter(
        is.na(DPA_SINGLE_LINE_ADDRESS) |
          is.na(GEO_SINGLE_LINE_ADDRESS) |
          DPA_SINGLE_LINE_ADDRESS == GEO_SINGLE_LINE_ADDRESS
      ),
    y = ab_db %>%
      filter(
        !is.na(DPA_SINGLE_LINE_ADDRESS),
        !is.na(GEO_SINGLE_LINE_ADDRESS),
        DPA_SINGLE_LINE_ADDRESS != GEO_SINGLE_LINE_ADDRESS
      ) %>%
      nhsbsaR::oracle_merge_strings(
        first_col = "DPA_SINGLE_LINE_ADDRESS",
        second_col = "GEO_SINGLE_LINE_ADDRESS",
        merge_col = "CORE_SINGLE_LINE_ADDRESS"
      )
  )

# Convert to a long table of distinct stacked single line addresses
ab_db <- ab_db %>%
  tidyr::pivot_longer(
    cols = ends_with("SINGLE_LINE_ADDRESS"),
    names_to = "ADDRESS_TYPE",
    values_to = "SINGLE_LINE_ADDRESS"
  ) %>%
  filter(!is.na(SINGLE_LINE_ADDRESS)) %>%
  distinct(POSTCODE, SINGLE_LINE_ADDRESS_LOOKUP = SINGLE_LINE_ADDRESS)

# Results

address_matched_db %>%
  filter(CH_FLAG == 1L) %>%
  inner_join(y = postcode_db) %>%
  left_join(y = addressbase_plus_db %>% mutate(AB = 1L)) %>%
  left_join(y = cqc_db %>% mutate(CQC = 1L)) %>%
  group_by(AB, CQC, MATCH_TYPE) %>%
  summarise(
    TOTAL_FORMS = sum(TOTAL_FORMS),
    TOTAL_PATIENTS = sum(TOTAL_PATIENTS)
  ) %>% 
  collect()

# Disconnect
DBI::dbDisconnect(con)