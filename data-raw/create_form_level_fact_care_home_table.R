library(dplyr)
library(dbplyr)

# Connections and Existing Table check

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Check if the table exists
exists <- DBI::dbExistsTable(conn = con, name = "INT615_PRESC_BASE")

# Drop any existing table beforehand
if (exists) DBI::dbRemoveTable(conn = con, name = "INT615_PRESC_BASE")

# Initial Lazy Tables from raw data

# 1. Create a lazy table from the year month table
year_month_db <- con %>%
  tbl(from = in_schema("DALL_REF", "YEAR_MONTH_DIM"))

# 2. Create a lazy table from the item level FACT table

fact_db <- con %>%
  tbl(from = in_schema("SB_AML", "PX_FORM_ITEM_ELEM_COMB_FACT"))

# 3. Create a lazy table from SCD2 payload message table 
eps_payload_messages_db <- con %>% 
  tbl(from = in_schema("SCD2", sql("SCD2_ETP_DY_PAYLOAD_MSG_DATA@dwcpb")))

# 4. Create a lazy table from the paper addresses table
paper_addresses_db <- con %>% 
  tbl(from = in_schema("DALL_REF", "INT615_PAPER_PFID_ADDRESS_20_21"))

# FACT table

# Filter to 2020/2021
year_month_db <- year_month_db %>%
  filter(FINANCIAL_YEAR == "2020/2021") %>%
  select(YEAR_MONTH)

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
  distinct(
    YEAR_MONTH,
    PART_DATE = EPS_PART_DATE,
    EPM_ID,
    PF_ID,
    NHS_NO,
    EPS_FLAG
  ) %>% 
  inner_join(y = year_month_db, by = "YEAR_MONTH") 
  
# EPS payload message data

# Subset the fact table for EPS
eps_fact_db <- fact_db %>%
  filter(EPS_FLAG == "Y")

# Create the single line address and subset columns
eps_payload_messages_db <- eps_payload_messages_db %>%
  # Buffer the end of part date by 6 days
  filter(
    PART_DATE >= 20200401L,
    PART_DATE <= 20210306L
  ) %>%
  # Concatenate fields together by a single space for the single line address
  mutate(
    SINGLE_LINE_ADDRESS = paste(
      PAT_ADDRESS_LINE1,
      PAT_ADDRESS_LINE2,
      PAT_ADDRESS_LINE3,
      PAT_ADDRESS_LINE4
    )
  ) %>%
  select(
    PART_DATE,
    EPM_ID,
    PAT_POSTCODE = PAT_ADDRESS_POSTCODE,
    PAT_ADDRESS = SINGLE_LINE_ADDRESS
  )

# Join back to the EPS forms FACT subset
eps_fact_db <- eps_fact_db %>%
  left_join(y = eps_payload_messages_db, by = c("EPM_ID", "PART_DATE")) %>%
  select(YEAR_MONTH, NHS_NO, PF_ID, PAT_ADDRESS, PAT_POSTCODE)

# Paper addresses data

# Subset the fact table for Paper
paper_fact_db <- fact_db %>%
  filter(EPS_FLAG == "N")

# Subset columns
paper_addresses_db <- paper_addresses_db %>%
  select(PF_ID, PAT_POSTCODE = POSTCODE, PAT_ADDRESS = ADDRESS)

# Join back to the paper forms FACT subset
paper_fact_db <- paper_fact_db %>%
  left_join(y = paper_addresses_db, by = "PF_ID") %>% 
  select(YEAR_MONTH, NHS_NO, PF_ID, PAT_ADDRESS, PAT_POSTCODE)

# Combined EPS and paper data with the FACT

# Stack EPS and paper back together
fact_db <- union_all(x = eps_fact_db, y = paper_fact_db)

# Tidy postcode and format single line addresses for tokenisation
fact_db <- fact_db %>%
  addressMatchR::tidy_postcode(col = PAT_POSTCODE) %>%
  addressMatchR::tidy_single_line_address(col = PAT_ADDRESS) %>% 
  mutate(ADDRESS_RECORD_ID = dense_rank(x = c("PAT_POSTCODE", "PAT_ADDRESS")))

# Write the table back to the DB
fact_db %>%
  nhsbsaR::oracle_create_table(table_name = "INT615_PRESC_BASE")

# Disconnect from database
DBI::dbDisconnect(con)

#-------------------------------------------------------------------------------