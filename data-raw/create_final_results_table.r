#-------------------------------------------------------------------------------

# Load library

library(magrittr)

# Set up connection to DALP

con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Prescribing base table

presc_base <- dplyr::tbl(
  src = con,
  from = dplyr::sql(
    "SELECT * FROM INT615_PRESC_BASE")
)

# Create a lazy table from the item level FACT table

results <- dplyr::tbl(
  src = con,
  from = dplyr::sql(
    "SELECT * FROM INT615_MATCH")
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
