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
  distinct()

# Get records with a single month with 5+ patients (in turn who are 65+)
five_plus <- presc_base %>%
  inner_join(y = postcodes, by = c("PAT_POSTCODE" = "AB_POSTCODE")) %>% 
  group_by(YEAR_MONTH, ADDRESS_RECORD_ID) %>%
  summarise(PATIENT_COUNT = n_distinct(NHS_NO)) %>%
  ungroup() %>%
  mutate(FIVE_PLUS = ifelse(PATIENT_COUNT >= 5, 1, 0)) %>%
  group_by(ADDRESS_RECORD_ID) %>%
  summarise(FIVE_PLUS = max(FIVE_PLUS)) %>%
  ungroup()

# Calculate key word columns
key_words <- presc_base %>% 
  select(ADDRESS_RECORD_ID, PAT_ADDRESS) %>% 
  distinct() %>% 
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
    CH_FLAG = ifelse(UPRN_COUNT >= 2, 0, CH_FLAG),
  ) %>% 
  left_join(y = five_plus, by = "ADDRESS_RECORD_ID") %>% 
  mutate(
    MATCH_TYPE = ifelse(CH_FLAG == 0 & FIVE_PLUS == 1, 'FIVE_PLUS', MATCH_TYPE),
    CH_FLAG == ifelse(MATCH_TYPE == 'FIVE_PLUS', 1, CH_FLAG)
    ) %>% 
  select(-c(UPRN_COUNT, FIVE_PLUS)) 

jw_matches

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
  left_join(y = five_plus, by = "ADDRESS_RECORD_ID") %>% 
  left_join(y = key_words, by = "ADDRESS_RECORD_ID") 








# Final Output
output <- distinct_records %>%
  left_join(y = max_monthly_patients, by = "ADDRESS_RECORD_ID")

#-------------------------------------------------------------------------------



# Create a lazy table from the item level FACT table

match <- dplyr::tbl(
  src = con,
  from = dplyr::sql(
    "SELECT * FROM ")
)

fact_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM SB_AML.PX_FORM_ITEM_ELEM_COMB_FACT")
)

# Create a lazy table from the year month table

year_month_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM DALL_REF.YEAR_MONTH_DIM")
)

# Functions

# Key word exclusions

key_word_exclusions = function(df, ADDRESS_FIELD){
  
  df = df %>% 
    dplyr::filter(
      INSTR({{ ADDRESS_FIELD }}, 'CHILDREN') == 0 &
        INSTR({{ ADDRESS_FIELD }}, 'MOBILE') == 0 &
        INSTR({{ ADDRESS_FIELD }}, 'ABOVE') == 0 &
        INSTR({{ ADDRESS_FIELD }}, 'CARAVAN') == 0 &
        INSTR({{ ADDRESS_FIELD }}, 'RESORT') == 0 &
        INSTR({{ ADDRESS_FIELD }}, 'CONVENT') == 0 &
        INSTR({{ ADDRESS_FIELD }}, 'MONASTERY') == 0 &
        INSTR({{ ADDRESS_FIELD }}, 'HOLIDAY') == 0 &
        INSTR({{ ADDRESS_FIELD }}, 'MARINA') == 0 &
        INSTR({{ ADDRESS_FIELD }}, 'RECOVERY') == 0 &
        INSTR({{ ADDRESS_FIELD }}, 'HOSPITAL') == 0 &
        INSTR({{ ADDRESS_FIELD }}, 'NO FIXED ABODE') == 0
    )
  return(df)
}

# Key word inclusions

key_word_inclusions = function(df, ADDRESS_FIELD){
  
  df = df %>% 
    dplyr::filter(
      INSTR({{ ADDRESS_FIELD }}, 'NURSING') != 0 |
        INSTR({{ ADDRESS_FIELD }}, 'RESIDENTIAL HOME') != 0 |
        INSTR({{ ADDRESS_FIELD }}, 'RESPITE') != 0 |
        INSTR({{ ADDRESS_FIELD }}, 'ELDERLY') != 0 |
        INSTR({{ ADDRESS_FIELD }}, 'CONVALESCENT') != 0 |
        INSTR({{ ADDRESS_FIELD }}, 'REST HOME') != 0 |
        INSTR({{ ADDRESS_FIELD }}, 'CARE HOME') != 0
    )
  return(df)
}

# Forms for matched records

match_pf = presc_base %>% 
  dplyr::select(ADDRESS_RECORD_ID, PF_ID, PAT_ADDRESS) %>% 
  dplyr::inner_join(
    results %>% 
      dplyr::filter(CAREHOME_FLAG == 1) %>% 
      dplyr::select(ADDRESS_RECORD_ID, UPRN, MATCH_TYPE, CAREHOME_FLAG),
    by = "ADDRESS_RECORD_ID"
  ) %>% 
  key_word_exclusions(., PAT_ADDRESS)

# Forms for non-matched records yet with 5+ pats aged 65+ at an address 

pat_pf = presc_base %>% 
  dplyr::select(PF_ID, YEAR_MONTH, PAT_ADDRESS, NHS_NO, ADDRESS_RECORD_ID) %>% 
  dplyr::inner_join(
    results %>% 
      dplyr::select(ADDRESS_RECORD_ID),
    by = "ADDRESS_RECORD_ID"
  ) %>% 
  # Remove forms from match_pf
  dplyr::anti_join(
    match_pf %>% 
      dplyr::select(PF_ID),
    by = "PF_ID"
  ) %>% 
  # Count distinct pats per address record per month (calculated monthly)
  dplyr::group_by(YEAR_MONTH, PAT_ADDRESS) %>% 
  dplyr::summarise(PAT_COUNT = dplyr::n_distinct(NHS_NO)) %>% 
  dplyr::ungroup() %>% 
  # 5+ monthly pats aged 65+ is a care home
  dplyr::filter(PAT_COUNT >= 5) %>% 
  # Counteract this with key word exclusions
  key_word_exclusions(., PAT_ADDRESS) %>% 
  # Get form info from base table
  dplyr::inner_join(
    presc_base %>% 
      dplyr::select(PF_ID, YEAR_MONTH, PAT_ADDRESS),
    by = c("YEAR_MONTH", "PAT_ADDRESS")
  ) %>% 
  # Only PF_ID needed from above fields
  dplyr::select(PF_ID) 
  # Mutate extra fields for later union_all alignment
  dplyr::mutate(
    CAREHOME_FLAG = 1,
    UPRN = NA,
    UPRN = as.integer(UPRN),
    MATCH_TYPE = 'PATIENT_COUNT',
  ) %>% 
  dplyr::select(PF_ID, CAREHOME_FLAG, UPRN, MATCH_TYPE)

# Forms for non-matched records yet with care home-related keyword

kw_pf = presc_base %>% 
  dplyr::select(PF_ID, PAT_ADDRESS) %>% 
  dplyr::anti_join(
    pat_pf %>% 
      dplyr::select(PF_ID),
    by = "PF_ID"
  ) %>% 
  dplyr::anti_join(
    match_pf %>% 
      dplyr::select(PF_ID),
    by = "PF_ID"
  ) %>% 
  key_word_inclusions(., PAT_ADDRESS) %>% 
  key_word_exclusions(., PAT_ADDRESS) %>% 
  dplyr::select(PF_ID) %>% 
  dplyr::mutate(
    CAREHOME_FLAG = 1,
    UPRN = NA,
    UPRN = as.integer(UPRN),
    MATCH_TYPE = 'KEY_WORD',
  ) %>% 
  dplyr::select(PF_ID, CAREHOME_FLAG, UPRN, MATCH_TYPE)

# Union together all sets of forms

all_pf = match_pf %>% 
  union_all(pat_pf) %>% 
  union_all(kw_pf)

# Filter to 2020/2021

year_month_db <- year_month_db %>%
  dplyr::filter(FINANCIAL_YEAR == "2020/2021") %>%
  dplyr::select(YEAR_MONTH)

# Filter to elderly patients in 2019/2020 and required columns

final_results <- fact_db %>%
  dplyr::inner_join(year_month_db) %>%
  dplyr::filter(
    # Elderly patients
    CALC_AGE >= 65,
    # Standard exclusions
    PAY_DA_END == "N", # excludes disallowed items
    PAY_ND_END == "N", # excludes not dispensed items
    PAY_RB_END == "N", # excludes referred back items
    CD_REQ == "N", # excludes controlled drug requisitions 
    OOHC_IND == 0, # excludes out of hours dispensing
    PRIVATE_IND == 0, # excludes private dispensers
    IGNORE_FLAG == "N" # excludes LDP dummy forms
  ) %>%
  dplyr::select(
    YEAR_MONTH,
    PART_DATE = EPS_PART_DATE,
    EPM_ID,
    PF_ID,
    PRESC_TYPE_PRNT,
    PRESC_ID_PRNT,
    ITEM_COUNT,
    ITEM_PAY_DR_NIC,
    ITEM_CALC_PAY_QTY,
    PATIENT_IDENTIFIED,
    PDS_GENDER,
    CALC_AGE,
    NHS_NO,
    CALC_PREC_DRUG_RECORD_ID,
    EPS_FLAG
  ) %>% 
  dplyr::left_join(all_pf, by = "PF_ID") %>% 
  dplyr::mutate(CAREHOME_FLAG = ifelse(is.na(CAREHOME_FLAG) == T, 0, 1))

#-------------------------------------------------------------------------------
