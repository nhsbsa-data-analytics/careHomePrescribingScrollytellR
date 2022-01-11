library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Check if the table exists
exists <- DBI::dbExistsTable(
  conn = con, 
  name = "INT615_ITEM_LEVEL_FACT_MATCHED_CARE_HOME"
)

# Drop any existing table beforehand
if (exists) {
  DBI::dbRemoveTable(
    conn = con, 
    name = "INT615_ITEM_LEVEL_FACT_MATCHED_CARE_HOME"
  )
}

# Initial lazy tables from database

# Create a lazy table from the item level FACT table
item_fact_db <- con %>%
  tbl(from = in_schema("AML", "PX_FORM_ITEM_ELEM_COMB_FACT"))

# Create a lazy table from the item level FACT table
form_fact_db <- con %>%
  tbl(from = "INT615_FORM_LEVEL_FACT_CARE_HOME")

# Create a lazy table from the matched patient address care home table
patient_address_match_db <- con %>%
  tbl(from = "INT615_ADDRESS_MATCHED_CARE_HOME")

# Create item level FACT table

# Filter to elderly patients in 2020/2021 and required columns
item_fact_db <- item_fact_db %>%
  filter(
    # Elderly patients in the period
    CALC_AGE >= 65L,
    YEAR_MONTH >= 202004L & YEAR_MONTH <= 202103L,
    # Standard exclusions
    PAY_DA_END == "N", # excludes disallowed items
    PAY_ND_END == "N", # excludes not dispensed items
    PAY_RB_END == "N", # excludes referred back items
    CD_REQ == "N", # excludes controlled drug requisitions 
    OOHC_IND == 0L, # excludes out of hours dispensing
    PRIVATE_IND == 0L, # excludes private dispensers
    IGNORE_FLAG == "N" # excludes LDP dummy forms
  ) %>%
  select(
    YEAR_MONTH,
    PF_ID,
    EPS_FLAG,
    PART_DATE = EPS_PART_DATE,
    EPM_ID,
    PATIENT_IDENTIFIED,
    NHS_NO,
    CALC_AGE,
    CALC_PREC_DRUG_RECORD_ID,
    ITEM_COUNT,
    ITEM_PAY_DR_NIC,
    ITEM_CALC_PAY_QTY
  )

# Now we join the columns of interest back to the fact table and fill the 
# care home flag and match type columns
item_fact_db <- item_fact_db %>%
  inner_join(
    y = form_fact_db,
    na_matches = "na", # Match NA to NA for PART_DATE and EPM_ID
    copy = TRUE
  ) %>%
  left_join(
    y = patient_address_match_db %>% 
      select(-c(MONTHS_5PLUS_PATIENTS, MAX_MONTHLY_PATIENTS)),
    copy = TRUE
  ) %>%
  tidyr::replace_na(list(CH_FLAG = 0L, MATCH_TYPE = "NO MATCH"))

# Write the table back to the DB
item_fact_db %>%
  nhsbsaR::oracle_create_table(
    table_name = "INT615_ITEM_LEVEL_FACT_MATCHED_CARE_HOME"
  )

# Disconnect from database
DBI::dbDisconnect(con)