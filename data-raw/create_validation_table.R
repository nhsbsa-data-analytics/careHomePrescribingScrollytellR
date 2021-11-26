# Load library
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Part One: Load Lazy Tables ---------------------------------------------------

# Matching Results
match <- con %>%
  tbl(from = in_schema("ADNSH", "CARE_HOME_MATCH"))

# Prescribing base table
presc_base <- con %>%
  tbl(from = in_schema("ADNSH", "FORM_LEVEL_CARE_HOME_FACT"))

# AddressBase base table
ab_base <- con %>%
  tbl(from = in_schema("ADNSH", "ADDRESSBASE_PLUS_CARE_HOME"))

# Part Two: Calculate metrics for additional rule logic  -----------------------

# Get postcodes where there is a CH
postcodes <- ab_base %>% 
  filter(CH_FLAG == 1) %>% 
  select(AB_POSTCODE) %>% 
  distinct() %>% 
  mutate(CH_POSTCODE = 1)

# Get records with a single month with 5+ patients (in turn who are 65+)
five_plus <- presc_base %>%
  group_by(YEAR_MONTH, ADDRESS_RECORD_ID, PAT_POSTCODE) %>%
  summarise(PATIENT_COUNT = n_distinct(NHS_NO)) %>%
  ungroup() %>%
  left_join(y = postcodes, by = c("PAT_POSTCODE" = "AB_POSTCODE")) %>% 
  mutate(
    FIVE_PLUS_MONTHS = ifelse(PATIENT_COUNT >= 5, 1, 0),
    FIVE_PLUS = ifelse(PATIENT_COUNT >= 5, 1, 0),
    CH_POSTCODE = ifelse(is.na(CH_POSTCODE), 0, 1)
    ) %>%
  group_by(ADDRESS_RECORD_ID, CH_POSTCODE) %>%
  summarise(
    FIVE_PLUS_MONTHS = sum(FIVE_PLUS_MONTHS),
    FIVE_PLUS = max(FIVE_PLUS)
    ) %>%
  ungroup()

# Calculate key word columns
total_record_info <- presc_base %>% 
  group_by(ADDRESS_RECORD_ID, PAT_ADDRESS) %>% 
  summarise(
    PATIENT_COUNT = n_distinct(NHS_NO),
    FORM_COUNT = n_distinct(PF_ID)
  ) %>% 
  ungroup() %>% 
  left_join(y = five_plus, by = "ADDRESS_RECORD_ID") %>%
  mutate(
    KW_EXCLSUIONS = case_when(
      INSTR(PAT_ADDRESS, 'CHILDREN') != 0 |
        INSTR(PAT_ADDRESS, 'MOBILE') != 0 |
        INSTR(PAT_ADDRESS, 'ABOVE') != 0 |
        INSTR(PAT_ADDRESS, 'CARAVAN') != 0 |
        INSTR(PAT_ADDRESS, 'RESORT') != 0 |
        INSTR(PAT_ADDRESS, 'CONVENT') != 0 |
        INSTR(PAT_ADDRESS, 'MONASTERY') != 0 |
        INSTR(PAT_ADDRESS, 'HOLIDAY') != 0 |
        INSTR(PAT_ADDRESS, 'MARINA') != 0 |
        INSTR(PAT_ADDRESS, 'RECOVERY') != 0 |
        INSTR(PAT_ADDRESS, 'HOSPITAL') != 0 ~ 1, T ~ 0
    ),
    KW_INCLUSIONS = case_when(
      INSTR(PAT_ADDRESS, 'NURSING HOME') != 0 |
        INSTR(PAT_ADDRESS, 'REST HOME') != 0 |
        INSTR(PAT_ADDRESS, 'CARE HOME') != 0 |
        INSTR(PAT_ADDRESS, 'RESIDENTIAL HOME') != 0 |
        INSTR(PAT_ADDRESS, 'ELDERLY') != 0 |
        INSTR(PAT_ADDRESS, 'CONVALESCENT') != 0 ~ 1, T ~ 0
    ),
    KW_CHECK = case_when(
      INSTR(PAT_ADDRESS, 'CHILDREN') != 0 |
        INSTR(PAT_ADDRESS, 'MOBILE') != 0 |
        INSTR(PAT_ADDRESS, 'ABOVE') != 0 |
        INSTR(PAT_ADDRESS, 'CARAVAN') != 0 |
        INSTR(PAT_ADDRESS, 'RESORT') != 0 |
        INSTR(PAT_ADDRESS, 'MONASTERY') != 0 |
        INSTR(PAT_ADDRESS, 'HOLIDAY') != 0 |
        INSTR(PAT_ADDRESS, 'MARINA') != 0 |
        INSTR(PAT_ADDRESS, 'RECOVERY') != 0 ~ 1, T ~ 0
    )
  ) %>% 
  select(-PAT_ADDRESS)

# Part Three: Resolve Matched with tied best score -----------------------------

# Get distinct UPRN and UPRN_ID combinations
ab_base <- ab_base %>% 
  select(UPRN, UPRN_ID, CLASS) %>% 
  mutate(CH_FLAG = ifelse(CLASS == 'RI01', 1, 0)) %>% 
  select(-CLASS)

# Resolve Records with Drawn Top Score
jw_matches <- match %>% 
  filter(MATCH_TYPE == 'JW') %>% 
  rename(
    ADDRESS_RECORD_ID = PRIMARY_ID,
    PAT_ADDRESS = PRIMARY_ADDRESS,
    UPRN_ID = LOOKUP_ID,
    AB_ADDRESS = LOOKUP_ADDRESS
  ) %>% 
  left_join(y = ab_base, by = "UPRN_ID") %>% 
  group_by(ADDRESS_RECORD_ID) %>% 
  mutate(UPRN_COUNT = n_distinct(UPRN)) %>% 
  ungroup() %>% 
  group_by(POSTCODE, ADDRESS_RECORD_ID, PAT_ADDRESS) %>% 
  summarise_all(.funs = max) %>% 
  ungroup() %>% 
  mutate(
    UPRN_ID = ifelse(UPRN_COUNT >= 2, NA, UPRN_ID),
    AB_ADDRESS = ifelse(UPRN_COUNT >= 2, NA, AB_ADDRESS),
    UPRN = ifelse(UPRN_COUNT >= 2, NA, UPRN),
    MATCH_TYPE = ifelse(UPRN_COUNT >= 2, 'NONE', MATCH_TYPE),
    MATCH_SCORE = ifelse(UPRN_COUNT >= 2, NA, MATCH_SCORE),
    CH_FLAG = ifelse(UPRN_COUNT >= 2, 0, CH_FLAG)
  ) %>% 
  select(-c(UPRN_COUNT))

# Process exact matches
exact_matches <- match %>% 
  filter(MATCH_TYPE == 'EXACT') %>% 
  rename(
    ADDRESS_RECORD_ID = PRIMARY_ID,
    PAT_ADDRESS = PRIMARY_ADDRESS,
    UPRN_ID = LOOKUP_ID,
    AB_ADDRESS = LOOKUP_ADDRESS
  ) %>% 
  left_join(y = ab_base, by = "UPRN_ID")

# Process non matches
non_matches <- match %>% 
  filter(MATCH_TYPE == 'NONE') %>% 
  rename(
    ADDRESS_RECORD_ID = PRIMARY_ID,
    PAT_ADDRESS = PRIMARY_ADDRESS,
    UPRN_ID = LOOKUP_ID,
    AB_ADDRESS = LOOKUP_ADDRESS
  ) %>% 
  mutate(
    UPRN = NA,
    CH_FLAG = 0
  )

# Part Four: Join all data and determine final CH_FLAG -------------------------

# Union all data and join patient count information
match <- jw_matches %>% 
  union_all(y = exact_matches) %>% 
  union_all(y = non_matches) %>% 
  left_join(y = total_record_info, by = "ADDRESS_RECORD_ID") %>% 
  mutate(
    MATCH_TYPE = ifelse(CH_FLAG == 0 & CH_POSTCODE == 1 & FIVE_PLUS  == 1, 'FIVE_PLUS', MATCH_TYPE),
    CH_FLAG = ifelse(CH_FLAG == 0 & CH_POSTCODE == 1 & FIVE_PLUS == 1, 1, CH_FLAG),
    MATCH_TYPE = ifelse(CH_FLAG == 0 & KW_INCLUSIONS == 1, 'KEY_WORD', MATCH_TYPE),
    CH_FLAG = ifelse(CH_FLAG == 0 & KW_INCLUSIONS == 1, 1, CH_FLAG),
    MATCH_TYPE = ifelse(KW_CHECK == 1, 'NONE', MATCH_TYPE),
    CH_FLAG = ifelse(KW_CHECK == 1, 0, CH_FLAG)
  )

# Write the table back to the DB (~30m)
match %>%
  nhsbsaR::oracle_create_table(table_name = "CARE_HOME_VALIDATION")

# Disconnect from database
DBI::dbDisconnect(con)

#-------------------------------------------------------------------------------
