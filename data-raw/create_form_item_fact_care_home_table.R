library(magrittr)

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the year month table
year_month_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM DALL_REF.YEAR_MONTH_DIM")
)

# Filter to 2019/2020
year_month_db <- year_month_db %>%
  dplyr::filter(FINANCIAL_YEAR == "2019/2020") %>%
  dplyr::select(YEAR_MONTH)

# Create a lazy table from the item level FACT table
fact_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM SB_AML.PX_FORM_ITEM_ELEM_COMB_FACT")
)

# Filter to elderly patients in 2019/2020 and required columns
fact_db <- fact_db %>%
  dplyr::inner_join(year_month_db) %>%
  dplyr::filter(
    # Identified elderly patients
    #PATIENT_IDENTIFIED == "Y",
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
  # Strip spaces from postcode
  dplyr::mutate(POSTCODE = REGEXP_REPLACE(PATIENT_ADDR_POSTCODE, " ", "")) %>%
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
    #POSTCODE,
    CALC_PREC_DRUG_RECORD_ID,
    EPS_FLAG
  )

# Subset the fact table for EPS / paper
eps_fact_db <- fact_db %>%
  dplyr::filter(EPS_FLAG == "Y")
paper_fact_db <- fact_db %>%
  dplyr::filter(EPS_FLAG == "N")

# We are only matching addresses for records with postcodes in our care home 
# table (where there is a care home in the postcode) so create the lazy table
addressbase_plus_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM ADDRESSBASE_PLUS_CARE_HOME")
)

# Get the postcodes where a care home is present
care_home_postcodes_db <- addressbase_plus_db %>%
  dplyr::distinct(POSTCODE)

# Create a lazy table from SCD2 payload message table 
eps_payload_messages_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM SCD2.SCD2_ETP_DY_PAYLOAD_MSG_DATA@dwcpb")
)

# Create the single line address and subset columns
eps_payload_messages_db <- eps_payload_messages_db %>%
  # Buffer the end of part date by 6 days
  dplyr::filter(
    PART_DATE >= 20200401,
    PART_DATE <= 20210306
  ) %>%
  dplyr::mutate(
    # Strip whitespace from postcode
    POSTCODE = REGEXP_REPLACE(PAT_ADDRESS_POSTCODE, " ", ""),
    # Concatenate fields together by a single space for the single line address
    SINGLE_LINE_ADDRESS = paste(
      PAT_ADDRESS_LINE1,
      PAT_ADDRESS_LINE2,
      PAT_ADDRESS_LINE3,
      PAT_ADDRESS_LINE4
    ),
    SINGLE_LINE_ADDRESS = toupper(SINGLE_LINE_ADDRESS),
    SINGLE_LINE_ADDRESS = TRIM(SINGLE_LINE_ADDRESS)
  ) %>%
  dplyr::select(PART_DATE, EPM_ID, POSTCODE, SINGLE_LINE_ADDRESS)

# Filter to postcodes with a care home
eps_payload_messages_db <- eps_payload_messages_db %>%
  dplyr::inner_join(care_home_postcodes_db)

# Join back to the EPS forms FACT subset
eps_fact_db <- eps_fact_db %>%
  dplyr::left_join(eps_payload_messages_db)

# Create a lazy table from the paper addresses table
paper_addresses_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM DALL_REF.INT615_PAPER_PFID_ADDRESS_20_21")
)

# Strip whitespace from postcode and subset columns
paper_addresses_db <- paper_addresses_db %>%
  dplyr::mutate(POSTCODE = REGEXP_REPLACE(POSTCODE, " ", "")) %>%
  dplyr::select(YEAR_MONTH, PF_ID, POSTCODE, SINGLE_LINE_ADDRESS = ADDRESS)

# Filter to postcodes with a care home
paper_addresses_db <- paper_addresses_db %>%
  dplyr::inner_join(care_home_postcodes_db)

# Join back to the paper forms FACT subset
paper_fact_db <- paper_fact_db %>%
  dplyr::left_join(paper_addresses_db)

# Stack EPS and paper back together
fact_db <- dplyr::union(x = eps_fact_db, paper_fact_db)

# Format single line addresses for tokenisation
fact_db <- fact_db %>%
  addressMatchR::tidy_single_line_address(col = SINGLE_LINE_ADDRESS)

# Write the table back to the DB
fact_db %>%
  nhsbsaR::oracle_create_table(
    table_name = "FORM_ITEM_FACT_CARE_HOME"
  )
library(magrittr)

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the year month table
year_month_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM DALL_REF.YEAR_MONTH_DIM")
)

# Filter to 2019/2020
year_month_db <- year_month_db %>%
  dplyr::filter(FINANCIAL_YEAR == "2019/2020") %>%
  dplyr::select(YEAR_MONTH)

# Create a lazy table from the item level FACT table
fact_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM SB_AML.PX_FORM_ITEM_ELEM_COMB_FACT")
)

# Filter to elderly patients in 2019/2020 and required columns
fact_db <- fact_db %>%
  dplyr::inner_join(year_month_db) %>%
  dplyr::filter(
    # Identified elderly patients
    #PATIENT_IDENTIFIED == "Y",
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
  # Strip spaces from postcode
  dplyr::mutate(POSTCODE = REGEXP_REPLACE(PATIENT_ADDR_POSTCODE, " ", "")) %>%
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
    #POSTCODE,
    CALC_PREC_DRUG_RECORD_ID,
    EPS_FLAG
  )

# Subset the fact table for EPS / paper
eps_fact_db <- fact_db %>%
  dplyr::filter(EPS_FLAG == "Y")
paper_fact_db <- fact_db %>%
  dplyr::filter(EPS_FLAG == "N")

# We are only matching addresses for records with postcodes in our care home 
# table (where there is a care home in the postcode) so create the lazy table
addressbase_plus_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM ADDRESSBASE_PLUS_CARE_HOME")
)

# Get the postcodes where a care home is present
care_home_postcodes_db <- addressbase_plus_db %>%
  dplyr::distinct(POSTCODE)

# Create a lazy table from SCD2 payload message table 
eps_payload_messages_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM SCD2.SCD2_ETP_DY_PAYLOAD_MSG_DATA@dwcpb")
)

# Create the single line address and subset columns
eps_payload_messages_db <- eps_payload_messages_db %>%
  # Buffer the end of part date by 6 days
  dplyr::filter(
    PART_DATE >= 20200401,
    PART_DATE <= 20210306
  ) %>%
  dplyr::mutate(
    # Strip whitespace from postcode
    POSTCODE = REGEXP_REPLACE(PAT_ADDRESS_POSTCODE, " ", ""),
    # Concatenate fields together by a single space for the single line address
    SINGLE_LINE_ADDRESS = paste(
      PAT_ADDRESS_LINE1,
      PAT_ADDRESS_LINE2,
      PAT_ADDRESS_LINE3,
      PAT_ADDRESS_LINE4
    ),
    SINGLE_LINE_ADDRESS = toupper(SINGLE_LINE_ADDRESS),
    SINGLE_LINE_ADDRESS = TRIM(SINGLE_LINE_ADDRESS)
  ) %>%
  dplyr::select(PART_DATE, EPM_ID, POSTCODE, SINGLE_LINE_ADDRESS)

# Filter to postcodes with a care home
eps_payload_messages_db <- eps_payload_messages_db %>%
  dplyr::inner_join(care_home_postcodes_db)

# Join back to the EPS forms FACT subset
eps_fact_db <- eps_fact_db %>%
  dplyr::left_join(eps_payload_messages_db)

# Create a lazy table from the paper addresses table
paper_addresses_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM DALL_REF.INT615_PAPER_PFID_ADDRESS_20_21")
)

# Strip whitespace from postcode and subset columns
paper_addresses_db <- paper_addresses_db %>%
  dplyr::mutate(POSTCODE = REGEXP_REPLACE(POSTCODE, " ", "")) %>%
  dplyr::select(YEAR_MONTH, PF_ID, POSTCODE, SINGLE_LINE_ADDRESS = ADDRESS)

# Filter to postcodes with a care home
paper_addresses_db <- paper_addresses_db %>%
  dplyr::inner_join(care_home_postcodes_db)

# Join back to the paper forms FACT subset
paper_fact_db <- paper_fact_db %>%
  dplyr::left_join(paper_addresses_db)

# Stack EPS and paper back together
fact_db <- dplyr::union(x = eps_fact_db, paper_fact_db)

# Format single line addresses for tokenisation
fact_db <- fact_db %>%
  addressMatchR::tidy_single_line_address(col = SINGLE_LINE_ADDRESS)

# Write the table back to the DB
fact_db %>%
  nhsbsaR::oracle_create_table(table_name = "FORM_ITEM_FACT_CARE_HOME")

# Disconnect from database
DBI::dbDisconnect(con)