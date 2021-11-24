# Load library
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Part One: Load Lazy Tables ---------------------------------------------------

# Initial Lazy Tables from raw data

# 1. Create a lazy table from the year month table
year_month_db <- con %>%
  tbl(from = in_schema("DALL_REF", "YEAR_MONTH_DIM"))

# 2. Create a lazy table from the item level FACT table
fact_db <- con %>%
  tbl(from = in_schema("SB_AML", "PX_FORM_ITEM_ELEM_COMB_FACT"))

# 3. Record Level data
results <- con %>%
  tbl(from = in_schema("ADNSH", "CARE_HOME_VALIDATION"))

# 4. Prescribing base table
presc_base <- con %>%
  tbl(from = in_schema("ADNSH", "FORM_LEVEL_CARE_HOME_FACT"))

# Part Two: Calculate metrics for additional rule logic  -----------------------

# Filter to 2020/2021
year_month_db <- year_month_db %>%
  filter(FINANCIAL_YEAR == "2020/2021") %>%
  select(YEAR_MONTH)

# Get CH records
results <- results %>% 
  filter(CH_FLAG == 1) %>% 
  select(ADDRESS_RECORD_ID, CH_FLAG, UPRN, MATCH_TYPE)

# Get CH PF_ID
presc_base <- presc_base %>% 
  select(PF_ID, ADDRESSS_RECORD_ID) %>% 
  inner_join(y = results, by = "ADDRESS_RECORD_ID")

# Filter to elderly patients in 2019/2020 and required columns
fact_db <- fact_db %>%
  filter(
    # Elderly patients
    CALC_AGE >= 65L,
    # Standard exclusions
    PAY_DA_END == "N", # excludes disallowed items
    PAY_ND_END == "N", # excludes not dispensed items
    PAY_RB_END == "N", # excludes referred back items
    CD_REQ == "N", # excludes controlled drug requisitions 
    OOHC_IND == 0L, # excludes out of hours dispensing
    PRIVATE_IND == 0L, # excludes private dispensers
    IGNORE_FLAG == "N" # excludes LDP dummy forms
  ) %>%
  inner_join(y = year_month_db, by = "YEAR_MONTH") %>% 
  select(
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
  dplyr::mutate(CH_FLAG = ifelse(is.na(CH_FLAG) == T, 0, 1))

# Write the table back to the DB (~30m)
fact_db %>%
  nhsbsaR::oracle_create_table(table_name = "CARE_HOME_FINAL")
  
# Disconnect from database
DBI::dbDisconnect(con)

#-------------------------------------------------------------------------------
