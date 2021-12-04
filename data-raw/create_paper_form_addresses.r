# Load library
library(dplyr)
library(dbplyr)

# Part 1: Temp Table creation in DWCP ------------------------------------------

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DWCP")

# 1. Create a lazy table from SCD2 payload message table 
eps_import_db <- con %>% 
  tbl(from = in_schema("SCD2", sql("SCD2_EXT_PD_IMPORT_DATA")))

# 2. Create a lazy table from the year month table
year_month_db <- con %>%
  tbl(from = in_schema("DIM", "YEAR_MONTH_DIM"))

# Edit Year month table for later join
year_month_db <- year_month_db %>%
  filter(YEAR_MONTH >= 201912 & YEAR_MONTH <= 202104) %>%
  select(YEAR_MONTH, YEAR_MONTH_ID)

# Create filtered version of eps_import_db
eps_import_db <- eps_import_db %>% 
  select(
    PART_MONTH,
    POSTCODE_R,
    LOCAL_PID_S,
    TRACE_RESULT_NEW_NHS_NUMBER_R,
    RECORD_NO,
    RECORD_TYPE_R,
    ADDRESS_LINE1_R,
    ADDRESS_LINE2_R,
    ADDRESS_LINE3_R,
    ADDRESS_LINE4_R
  ) %>% 
  inner_join(y = year_month_db, by = c("PART_MONTH" = "YEAR_MONTH")) %>% 
  filter(
    RECORD_TYPE_R == '20' |
      RECORD_TYPE_R == '30' |
      RECORD_TYPE_R == '40' |
      RECORD_TYPE_R == '33'
  ) %>% 
  mutate(
    NHS_NO_PDS = TRACE_RESULT_NEW_NHS_NUMBER_R,
    YEAR_MONTH = substr(LOCAL_PID_S, 1, 6),
    ADDRESS_R = upper(trim(paste0(
      ADDRESS_LINE1_R, ', ',
      ADDRESS_LINE2_R, ', ',
      ADDRESS_LINE3_R, ', ',
      ADDRESS_LINE4_R)))
  )%>% 
  select(
    PART_MONTH,
    YEAR_MONTH,
    ADDRESS_R,
    POSTCODE_R,
    NHS_NO_PDS,
    RECORD_NO
  )

# Write the table back to the DB
eps_import_db %>%
  nhsbsaR::oracle_create_table(table_name = "CARE_HOME_SCD2_IMPORT_FILTER")

# Grant Access
# grant select on adnsh.CARE_HOME_SCD2_IMPORT_FILTER to DALP_USER;

# Disconnect from database
DBI::dbDisconnect(con)

# Part 2: Temp Table creation in DALL ------------------------------------------

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from SCD2 payload message table 
eps_payload_db <- con %>% 
  tbl(from = in_schema("SCD2", sql("SCD2_ETP_DY_PAYLOAD_MSG_DATA@dwcpb")))

# Create a lazy table from SCD2 payload message table 
eps_import_db <- con %>% 
  tbl(from = in_schema("ADNSH", sql("CARE_HOME_SCD2_IMPORT_FILTER@dwcpb")))

# Save Filtered SCD2
eps_import_db %>%
  nhsbsaR::oracle_create_table(table_name = "CARE_HOME_SCD2_IMPORT_FILTER")

# Filter SCD2
eps_payload_filter <- eps_payload_db %>% 
  select(
    PAT_ADDRESS_LINE1,
    PAT_ADDRESS_LINE2,
    PAT_ADDRESS_LINE3,
    PAT_ADDRESS_LINE4,
    EPM_ID,
    PART_DATE
  ) %>% 
  mutate(
    PATIENT_ADDR_FULL = upper(trim(paste0(
      PAT_ADDRESS_LINE1, ', ',
      PAT_ADDRESS_LINE2, ', ',
      PAT_ADDRESS_LINE3, ', ',
      PAT_ADDRESS_LINE4))),
    YEAR_MONTH = substr(PART_DATE, 1, 6)
  ) %>% 
  inner_join(
    y = year_month_wide_db %>% select(YEAR_MONTH),
    by = "YEAR_MONTH"
  ) %>% 
  select(
    PATIENT_ADDR_FULL,
    EPM_ID,
    PART_DATE
  )

# Save Filtered SCD2
eps_payload_filter %>%
  nhsbsaR::oracle_create_table(table_name = "CARE_HOME_SCD2_PAYLOAD_FILTER")

# Disconnect from database
DBI::dbDisconnect(con)

# Part 3.1: Base tables for final script ---------------------------------------

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Initial Lazy Tables from raw data

# 1. Create a lazy table from the year month table
year_month_db <- con %>%
  tbl(from = in_schema("DALL_REF", "YEAR_MONTH_DIM"))

# 2. Create a lazy table from the item level FACT table
fact_db <- con %>%
  tbl(from = in_schema("AML", "PX_FORM_ITEM_ELEM_COMB_FACT"))

# 3. Create a lazy table from Temp SCD2 Saved Table
eps_payload_db <- con %>% 
  tbl(from = in_schema("ADNSH", sql("CARE_HOME_SCD2_PAYLOAD_FILTER")))

# 4. Create a lazy table from SCD2 import data table 
eps_import_db <- con %>% 
  tbl(from = in_schema("ADNSH", sql("CARE_HOME_SCD2_IMPORT_FILTER")))

# 5. Create a lazy table from the CIP patient dim table
cip_db <- con %>%
  tbl(from = in_schema("DIM", "CIP_PATIENT_DIM"))

# Part 3.2 Base Tables Initial Edit --------------------------------------------

# Filter to several months outside 2020/2021
year_month_extra_wide_db <- year_month_db %>%
  filter(YEAR_MONTH >= 201912 & YEAR_MONTH <= 202104) %>%
  select(YEAR_MONTH, YEAR_MONTH_ID)

# Filter to several months outside 2020/2021
year_month_wide_db <- year_month_db %>%
  filter(YEAR_MONTH >= 202002 & YEAR_MONTH <= 202105) %>%
  select(YEAR_MONTH, YEAR_MONTH_ID)

# Filter to 2020/2021
year_month_db <- year_month_db %>%
  filter(FINANCIAL_YEAR == "2020/2021") %>%
  select(YEAR_MONTH, YEAR_MONTH_ID)

# Just get relevant CIP data
cip_db <- cip_db %>% 
  select(NHS_NO_PDS, NHS_NO_CIP)

# Part 3.3: Address Level Information Data Processing -------------------------

# Subset of data to check if PDS data can be aligned
px_data <- fact_db %>% 
  select(
    EPS_FLAG,
    PAY_DA_END,
    PAY_ND_END,
    PAY_RB_END,
    CD_REQ,
    OOHC_IND,
    PRIVATE_IND,
    IGNORE_FLAG,
    YEAR_MONTH,
    NHS_NO
  ) %>% 
  filter(
    # EPS Flag
    EPS_FLAG == 'N',    # paper prescriptions only
    PAY_DA_END == "N", # excludes disallowed items
    PAY_ND_END == "N", # excludes not dispensed items
    PAY_RB_END == "N", # excludes referred back items
    CD_REQ == "N", # excludes controlled drug requisitions 
    OOHC_IND == 0L, # excludes out of hours dispensing
    PRIVATE_IND == 0L, # excludes private dispensers
    IGNORE_FLAG == "N" # excludes LDP dummy forms
  ) %>% 
  select(NHS_NO_CIP = NHS_NO, YEAR_MONTH) %>% 
  inner_join(y = cip_db, by = "NHS_NO_CIP") %>% 
  inner_join(y = year_month_db, by = "YEAR_MONTH") %>% 
  distinct() %>% 
  mutate(
    #NHS_NO_PDS = as.character(NHS_NO_PDS),
    YEAR_MONTH_ID_M4 = YEAR_MONTH_ID - 4,
    YEAR_MONTH_ID_M3 = YEAR_MONTH_ID - 3,
    YEAR_MONTH_ID_M2 = YEAR_MONTH_ID - 2,
    YEAR_MONTH_ID_0  = YEAR_MONTH_ID,
    YEAR_MONTH_ID_M1 = YEAR_MONTH_ID - 1,
    YEAR_MONTH_ID_P1 = YEAR_MONTH_ID + 1,
    YEAR_MONTH_ID_P2 = YEAR_MONTH_ID + 2
    )

# Find Single ETP Addresses per Month Instances
etp_multi_address <- fact_db %>% 
  select(
    EPS_FLAG,
    PAY_DA_END,
    PAY_ND_END,
    PAY_RB_END,
    CD_REQ,
    OOHC_IND,
    PRIVATE_IND,
    IGNORE_FLAG,
    YEAR_MONTH,
    NHS_NO,
    PATIENT_ADDR_POSTCODE
  ) %>% 
  filter(
    # EPS Flag & standard exclusions
    PAY_DA_END == "N", # excludes disallowed items
    PAY_ND_END == "N", # excludes not dispensed items
    PAY_RB_END == "N", # excludes referred back items
    CD_REQ == "N", # excludes controlled drug requisitions 
    OOHC_IND == 0L, # excludes out of hours dispensing
    PRIVATE_IND == 0L, # excludes private dispensers
    IGNORE_FLAG == "N" # excludes LDP dummy forms
  ) %>% 
  inner_join(y = year_month_wide_db, by = "YEAR_MONTH") %>%
  group_by(YEAR_MONTH, NHS_NO) %>% 
  summarise(ADDRESS_COUNT = n_distinct(PATIENT_ADDR_POSTCODE)) %>% 
  ungroup() %>% 
  filter(ADDRESS_COUNT == 1) %>% 
  select(-ADDRESS_COUNT)

# ETP data
etp_data <- fact_db %>% 
  select(
    EPS_FLAG,
    EPM_ID,
    EPS_PART_DATE,
    YEAR_MONTH,
    NHS_NO,
    ITEM_COUNT,
    PATIENT_ADDR_POSTCODE
  ) %>% 
  filter(EPS_FLAG == 'Y') %>% 
  inner_join(y = year_month_wide_db, by = "YEAR_MONTH") %>% 
  inner_join(y = etp_multi_address, by = c("YEAR_MONTH", "NHS_NO")) %>% 
  inner_join(y = cip_db, by = c("NHS_NO" = "NHS_NO_CIP")) %>% 
  inner_join(y = eps_payload_db, by = c("EPM_ID", "EPS_PART_DATE" = "PART_DATE")) %>% 
  group_by(
    YEAR_MONTH,
    YEAR_MONTH_ID,
    NHS_NO_PDS,
    PATIENT_ADDR_FULL,
    PATIENT_ADDR_POSTCODE
    ) %>%
  summarise(ITEM_COUNT = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  group_by(YEAR_MONTH, NHS_NO_PDS) %>%                                          # ... rank() over (partition by YEAR_MONTH, NHS_NO_PDS
  mutate(RNK = rank(
    c(desc(ITEM_COUNT), PATIENT_ADDR_POSTCODE, PATIENT_ADDR_FULL))              # ... order by ITEM_COUNT desc, PATIENT_ADDR_POSTCODE, PATIENT_ADDR_FULL
    ) %>% 
  ungroup() %>% 
  filter(RNK == 1) %>% 
  select(YEAR_MONTH_ID, NHS_NO_PDS, PATIENT_ADDR_FULL, PATIENT_ADDR_POSTCODE)

# PDS Data
pds_data <- eps_import_db %>% 
  group_by(PART_MONTH, NHS_NO_PDS) %>% 
  mutate(RNK = rank(desc(RECORD_NO))) %>%
  ungroup() %>% 
  filter(RNK == 1) %>% 
  inner_join(y = year_month_wide_db, by = "YEAR_MONTH") %>% 
  select(YEAR_MONTH_ID, NHS_NO_PDS, ADDRESS_R, POSTCODE_R)

# Functions to join and rename columns easily

# Left join to PDS data
pds_left_join = function(df, year_num){
  
  year_month_col = paste0("YEAR_MONTH_ID", year_num)
  address_col = paste0("ADDRESS_R", year_num)
  postcode_col = paste0("POSTCODE_R", year_num)

  df %>% 
    left_join(
      pds_data %>% 
        rename_at("ADDRESS_R", ~address_col) %>% 
        rename_at("POSTCODE_R", ~postcode_col) %>% 
        rename_at("YEAR_MONTH_ID", ~year_month_col)
      )
}

# Left join to ETP data
etp_left_join = function(df, year_num){
  
  year_month_col = paste0("YEAR_MONTH_ID", year_num)
  address_col = paste0("PATIENT_ADDR_FULL", year_num)
  postcode_col = paste0("PATIENT_ADDR_POSTCODE", year_num)
  
  df %>% 
    left_join(
      etp_data %>% 
        rename_at("PATIENT_ADDR_FULL", ~address_col) %>% 
        rename_at("PATIENT_ADDR_POSTCODE", ~postcode_col) %>%
        rename_at("YEAR_MONTH_ID", ~year_month_col)
      )
}

# Address and Postcode Final Derivation
address_info <- px_data %>% 
  # Function joins
  pds_left_join(., year_num = "_M4") %>%
  pds_left_join(., year_num = "_M3") %>% 
  pds_left_join(., year_num = "_M2") %>% 
  pds_left_join(., year_num = "_M1") %>% 
  pds_left_join(., year_num = "_0") %>% 
  etp_left_join(., year_num = "_M2") %>% 
  etp_left_join(., year_num = "_M1") %>% 
  etp_left_join(., year_num = "_0") %>% 
  etp_left_join(., year_num = "_P1") %>%
  etp_left_join(., year_num = "_P2") %>%
  # Coalesce data
  mutate(
    ADDRESS = coalesce(
      PATIENT_ADDR_FULL_0,
      ADDRESS_R_M2,
      PATIENT_ADDR_FULL_M1,
      PATIENT_ADDR_FULL_P1,
      ADDRESS_R_M3,
      ADDRESS_R_M1,
      PATIENT_ADDR_FULL_M2,
      PATIENT_ADDR_FULL_P2,
      ADDRESS_R_M4,
      ADDRESS_R_0
    ),
    POSTCODE = coalesce(
      PATIENT_ADDR_POSTCODE_0,
      POSTCODE_R_M2,
      PATIENT_ADDR_POSTCODE_M1,
      PATIENT_ADDR_POSTCODE_P1,
      POSTCODE_R_M3,
      POSTCODE_R_M1,
      PATIENT_ADDR_POSTCODE_M2,
      PATIENT_ADDR_POSTCODE_P2,
      POSTCODE_R_M4,
      POSTCODE_R_0
    )
  ) %>% 
  # Final cols
  select(
    YEAR_MONTH,
    NHS_NO_CIP,
    NHS_NO_PDS,
    ADDRESS,
    POSTCODE
  )

# Save Address Information
Sys.time()
address_info %>%
  nhsbsaR::oracle_create_table(table_name = "CARE_HOME_ADDRESS_INFO")
Sys.time()

# Disconnect from database
DBI::dbDisconnect(con)

# Part Six: Form Level Information Data Processing -----------------------------

# 1. Create a lazy table from the year month table
year_month_db <- con %>%
  tbl(from = in_schema("DALL_REF", "YEAR_MONTH_DIM"))

# 2. Create a lazy table from the item level FACT table
fact_db <- con %>%
  tbl(from = in_schema("AML", "PX_FORM_ITEM_ELEM_COMB_FACT"))

# 1. Create a lazy table from the year month table
address_info_db <- con %>%
  tbl(from = in_schema("ADNSH", "CARE_HOME_ADDRESS_INFO"))

# final data
final_data <- fact_db %>% 
  select(
    # Required
    YEAR_MONTH,
    PF_ID,
    NHS_NO,
    # For Filter
    EPS_FLAG,
    PAY_DA_END,
    PAY_ND_END,
    PAY_RB_END,
    CD_REQ,
    OOHC_IND,
    PRIVATE_IND,
    IGNORE_FLAG
  ) %>% 
  filter(
    # EPS Flag & standard exclusions
    EPS_FLAG == 'N',
    PAY_DA_END == "N", # excludes disallowed items
    PAY_ND_END == "N", # excludes not dispensed items
    PAY_RB_END == "N", # excludes referred back items
    CD_REQ == "N", # excludes controlled drug requisitions 
    OOHC_IND == 0L, # excludes out of hours dispensing
    PRIVATE_IND == 0L, # excludes private dispensers
    IGNORE_FLAG == "N" # excludes LDP dummy forms
  ) %>% 
  inner_join(y = year_month_db, by = "YEAR_MONTH") %>% 
  inner_join(y = address_info, by = c("NHS_NO" = "NHS_NO_CIP", "YEAR_MONTH")) %>% 
  select(
    YEAR_MONTH,
    PF_ID,
    NHS_NO,
    NHS_NO_PDS,
    ADDRESS,
    POSTCODE
    ) %>% 
  # Filter NA addresses 
  filter(!is.na(ADDRESS)) %>% 
  distinct() %>% 
  mutate(ADDRESS = trimws(REGEXP_REPLACE(REPLACE(ADDRESS, ',', ' '), '( ){2,}', ' '))) 
  
# Save Address Information
final_data %>%
  nhsbsaR::oracle_create_table(table_name = "CARE_HOME_PAPER_ADDRESS")

# Disconnect from database
DBI::dbDisconnect(con)
  
#-------------------------------------------------------------------------------