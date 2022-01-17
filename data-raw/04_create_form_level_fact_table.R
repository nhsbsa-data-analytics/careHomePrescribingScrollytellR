library(dplyr)
library(dbplyr)

# Set up connection to DWCP and DALP
con_dwcp <- nhsbsaR::con_nhsbsa(database = "DWCP")
con_dalp <- nhsbsaR::con_nhsbsa(database = "DALP")

# Check if the table exists
exists <- con_dalp %>%
  DBI::dbExistsTable(name = "INT615_FORM_LEVEL_FACT")

# Drop any existing table beforehand
if (exists) {
  con_dalp %>%
    DBI::dbRemoveTable(name = "INT615_FORM_LEVEL_FACT")
}

# EPS payload message data

# First we have to create a filtered version of EPS payload message data in DALP

# Check if the table exists DALP
exists_dalp_eps_payload <- con_dalp %>%
  DBI::dbExistsTable(name = "INT615_SCD2_ETP_DY_PAYLOAD_MSG_DATA")

# Drop any existing table beforehand
if (exists_dalp_eps_payload) {
  con_dalp %>%
    DBI::dbRemoveTable(name = "INT615_SCD2_ETP_DY_PAYLOAD_MSG_DATA")
}

# Create a lazy table from SCD2 payload message table
eps_payload_messages_db <- con_dalp %>% 
  tbl(from = in_schema("SCD2", sql("SCD2_ETP_DY_PAYLOAD_MSG_DATA@dwcpb")))

# Create the single line address and subset columns
eps_payload_messages_db <- eps_payload_messages_db %>%
  # Bring back ETP data from the month previous until 2 months after (and buffer 
  # the end of part date by 10 days
  filter(
    PART_DATE >= 20200201L,
    PART_DATE <= 20210610L
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
    POSTCODE = PAT_ADDRESS_POSTCODE, 
    SINGLE_LINE_ADDRESS
  )

# Tidy postcode and format single line addresses
eps_payload_messages_db <- eps_payload_messages_db %>%
  addressMatchR::tidy_postcode(col = POSTCODE) %>%
  addressMatchR::tidy_single_line_address(col = SINGLE_LINE_ADDRESS)

# Write the table back to DALP with indexes
eps_payload_messages_db <- eps_payload_messages_db %>%
  compute(
    name = "INT615_SCD2_ETP_DY_PAYLOAD_MSG_DATA",
    indexes = list(c("PART_DATE", "EPM_ID"), c("POSTCODE")),
    temporary = FALSE
  )

# PDS trace data

# First we have to create a filtered version of PDS import data in DALP

# Check if the table exists DWCP and DALP
exists_dalp_pds_import <- con_dalp %>%
  DBI::dbExistsTable(name = "INT615_SCD2_EXT_PD_IMPORT_DATA")

# Drop any existing table beforehand
if (exists_dalp_pds_import) {
  con_dalp %>%
    DBI::dbRemoveTable(name = "INT615_SCD2_EXT_PD_IMPORT_DATA")
}

# Create a lazy table from year month dim table in DWCP
year_month_db <- con_dwcp %>% 
  tbl(from = in_schema("DIM", "YEAR_MONTH_DIM")) %>%
  select(YEAR_MONTH_ID, YEAR_MONTH)

# Create a lazy table from SCD2 PDS import data table in DWCP
pds_import_db <- con_dwcp %>% 
  tbl(from = in_schema("SCD2", "SCD2_EXT_PD_IMPORT_DATA"))

# Create a lazy table from the CIP patient dim table in DWCP
cip_db <- con_dwcp %>%
  tbl(from = in_schema("DIM", "CIP_PATIENT_DIM"))

# Filter to successful traces in the period of interest
pds_import_db <- pds_import_db %>% 
  filter(
    RECORD_TYPE_R %in% c("20", "30", "33", "40"),
    PART_MONTH >= 201912L & PART_MONTH <= 202104L
  )

# Extract the year month and single line address
pds_import_db <- pds_import_db %>%
  mutate(
    SINGLE_LINE_ADDRESS = paste(
      ADDRESS_LINE1_R,
      ADDRESS_LINE2_R,
      ADDRESS_LINE3_R,
      ADDRESS_LINE4_R, 
      ADDRESS_LINE5_R
    )
  ) 

# Select the columns of interest
pds_import_db <- pds_import_db %>%
  select(
    YEAR_MONTH = PART_MONTH, 
    SINGLE_LINE_ADDRESS, 
    POSTCODE = POSTCODE_R, 
    NHS_NO_PDS = TRACE_RESULT_NEW_NHS_NUMBER_R,
    RECORD_NO
  )

# Keep the latest record for each year month and nhs number
pds_import_db <- pds_import_db %>%
  group_by(YEAR_MONTH, NHS_NO_PDS) %>%
  slice_max(order_by = RECORD_NO, with_ties = FALSE) %>%
  ungroup() %>%
  select(-RECORD_NO)

# Join the year month details (shift by 1 month from part month)
pds_import_db <- pds_import_db %>%
  inner_join(y = year_month_db) %>%
  select(-YEAR_MONTH) %>%
  mutate(YEAR_MONTH_ID = YEAR_MONTH_ID + 1L) %>% 
  inner_join(y = year_month_db) %>%
  relocate(YEAR_MONTH_ID, YEAR_MONTH)

# Join the NHS_NO on so we can join to the FACT table
pds_import_db <- pds_import_db %>%
  inner_join(y = cip_db %>% select(NHS_NO_PDS, NHS_NO = NHS_NO_CIP))

# Tidy postcode and format single line addresses
pds_import_db <- pds_import_db %>%
  addressMatchR::tidy_postcode(col = POSTCODE) %>%
  addressMatchR::tidy_single_line_address(col = SINGLE_LINE_ADDRESS)

# Write the table back to DWCP with indexes
pds_import_db <- pds_import_db %>%
  compute(
    name = "INT615_SCD2_EXT_PD_IMPORT_DATA",
    indexes = list(c("YEAR_MONTH_ID", "YEAR_MONTH", "NHS_NO"), c("POSTCODE")),
    temporary = FALSE
  )

# Grant Access to DALP_USER
con_dwcp %>%
  DBI::dbSendStatement(
    statement = "GRANT SELECT ON INT615_SCD2_EXT_PD_IMPORT_DATA TO DALP_USER"
  )

# Create a lazy table in DALP from filtered version of SCD2_EXT_PD_IMPORT_DATA
# in DWCP
pds_import_db <- con_dalp %>% 
  tbl(
    from = in_schema(
      schema = sql(Sys.getenv("DB_DWCP_USERNAME")), 
      table = sql("INT615_SCD2_EXT_PD_IMPORT_DATA@dwcpb")
    )
  )

# Write the table back to DALP with indexes
pds_import_db <- pds_import_db %>%
  compute(
    name = "INT615_SCD2_EXT_PD_IMPORT_DATA",
    indexes = list(c("YEAR_MONTH", "NHS_NO"), c("POSTCODE")),
    temporary = FALSE
  )

# Drop the table from DWCP
con_dwcp %>%
  DBI::dbRemoveTable(name = "INT615_SCD2_EXT_PD_IMPORT_DATA")

# Disconnect from DWCP
DBI::dbDisconnect(con_dwcp)

# Pull relevant data from FACT table for the period

# Create a lazy table from the year month table
year_month_db <- con_dalp %>%
  tbl(from = in_schema("DALL_REF", "YEAR_MONTH_DIM")) %>%
  select(YEAR_MONTH_ID, YEAR_MONTH)

# Create a lazy table from the item level FACT table
fact_db <- con_dalp %>%
  tbl(from = in_schema("AML", "PX_FORM_ITEM_ELEM_COMB_FACT"))

# Standard exclusions on the FACT table
fact_db <- fact_db %>%
  filter(
    PAY_DA_END == "N", # excludes disallowed items
    PAY_ND_END == "N", # excludes not dispensed items
    PAY_RB_END == "N", # excludes referred back items
    CD_REQ == "N", # excludes controlled drug requisitions 
    OOHC_IND == 0L, # excludes out of hours dispensing
    PRIVATE_IND == 0L, # excludes private dispensers
    IGNORE_FLAG == "N" # excludes LDP dummy forms
  )

# Subset the columns
fact_db <- fact_db %>%
  select(
    YEAR_MONTH,
    PF_ID,
    EPS_FLAG,
    PART_DATE = EPS_PART_DATE,
    EPM_ID,
    PDS_GENDER,
    CALC_AGE,
    PATIENT_IDENTIFIED,
    NHS_NO,
    ITEM_COUNT
  )

# Get the elderly patients in 2020/2021
elderly_nhs_no_db <- fact_db %>%
  filter(
    CALC_AGE >= 65L,
    YEAR_MONTH >= 202004L,
    YEAR_MONTH <= 202103L
  ) %>%
  select(NHS_NO)

# Join the year month information
fact_db <- fact_db %>%
  inner_join(y = year_month_db) %>%
  relocate(YEAR_MONTH_ID)

# Subset the paper forms
paper_fact_db <- fact_db %>%
  filter(
    EPS_FLAG == "N",
    YEAR_MONTH >= 202004L,
    YEAR_MONTH <= 202103L,
    CALC_AGE >= 65L
  )

# Subset the EPS forms (buffer the period so that we can search for addresses 
# for paper forms from more months of EPS forms) to elderly patients in 2020/21
# and join on their addresses
eps_fact_db <- fact_db %>%
  filter(
    EPS_FLAG == "Y",
    YEAR_MONTH >= 202002L,
    YEAR_MONTH <= 202105L
  ) %>%
  semi_join(y = elderly_nhs_no_db) %>%
  left_join(y = eps_payload_messages_db)

# Get EPS addresses

# Get a single postcode and address per EPS patient
eps_single_address_db <- eps_fact_db %>%
  filter(!is.na(POSTCODE)) %>%
  # Remove patients with multiple postcodes in the same month
  group_by(YEAR_MONTH_ID, YEAR_MONTH, NHS_NO) %>%
  mutate(POSTCODE_COUNT = n_distinct(POSTCODE)) %>%
  filter(POSTCODE_COUNT == 1) %>%
  select(-POSTCODE_COUNT) %>%
  # And keep their address with the biggest item count in each postcode 
  group_by(POSTCODE, SINGLE_LINE_ADDRESS, .add = TRUE) %>%
  summarise(ITEM_COUNT = sum(ITEM_COUNT)) %>%
  ungroup(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  slice_max(order_by = ITEM_COUNT, with_ties = FALSE) %>%
  ungroup()

# Get the PDS patients that we need to find an address for

# Get the patients that we want an address for
paper_patient_db <- paper_fact_db %>% 
  distinct(YEAR_MONTH_ID, YEAR_MONTH, NHS_NO)

# Add the year month information
paper_patient_db <- paper_patient_db %>%
  mutate(
    YEAR_MONTH_ID_M2 = YEAR_MONTH_ID - 2L,
    YEAR_MONTH_ID_M1 = YEAR_MONTH_ID - 1L,
    YEAR_MONTH_ID_P1 = YEAR_MONTH_ID + 1L,
    YEAR_MONTH_ID_P2 = YEAR_MONTH_ID + 2L
  )

# Define a function that will make the joining process easier
left_join_address <- function(x, y, year_month_id_col, suffix){
  
  left_join(
    x = x,
    y = y %>%
      rename(
        "{{year_month_id_col}}" := YEAR_MONTH_ID,
        "POSTCODE_{{year_month_id_col}}_{{suffix}}" := POSTCODE,
        "SINGLE_LINE_ADDRESS_{{year_month_id_col}}_{{suffix}}" := 
          SINGLE_LINE_ADDRESS
      ),
    by = c(rlang::as_name(rlang::enquo(year_month_id_col)), "NHS_NO")
  )
  
}

# Join the ETP / PDS postcode and addresses for each year month ID and coalesce
# to get the most appropriate postcode and address
paper_patient_db <- paper_patient_db %>%
  left_join_address(
    y = eps_single_address_db,
    year_month_id_col = YEAR_MONTH_ID,
    suffix = EPS
  ) %>%
  left_join_address(
    y = pds_import_db,
    year_month_id_col = YEAR_MONTH_ID,
    suffix = PDS
  ) %>%
  left_join_address(
    y = eps_single_address_db,
    year_month_id_col = YEAR_MONTH_ID_M1,
    suffix = EPS
  ) %>%
  left_join_address(
    y = eps_single_address_db,
    year_month_id_col = YEAR_MONTH_ID_P1,
    suffix = EPS
  ) %>%
  left_join_address(
    y = pds_import_db,
    year_month_id_col = YEAR_MONTH_ID_M1,
    suffix = PDS
  ) %>%
  left_join_address(
    y = pds_import_db,
    year_month_id_col = YEAR_MONTH_ID_P1,
    suffix = PDS
  ) %>%
  left_join_address(
    y = eps_single_address_db,
    year_month_id_col = YEAR_MONTH_ID_M2,
    suffix = EPS
  ) %>%
  left_join_address(
    y = eps_single_address_db,
    year_month_id_col = YEAR_MONTH_ID_P2,
    suffix = EPS
  ) %>%
  left_join_address(
    y = pds_import_db,
    year_month_id_col = YEAR_MONTH_ID_M2,
    suffix = PDS
  ) %>%
  left_join_address(
    y = pds_import_db,
    year_month_id_col = YEAR_MONTH_ID_P2,
    suffix = PDS
  ) %>%
  mutate(
    POSTCODE = coalesce(
      POSTCODE_YEAR_MONTH_ID_EPS,
      POSTCODE_YEAR_MONTH_ID_PDS,
      POSTCODE_YEAR_MONTH_ID_M1_EPS,
      POSTCODE_YEAR_MONTH_ID_P1_EPS,
      POSTCODE_YEAR_MONTH_ID_M1_PDS,
      POSTCODE_YEAR_MONTH_ID_P1_PDS,
      POSTCODE_YEAR_MONTH_ID_M2_EPS,
      POSTCODE_YEAR_MONTH_ID_P2_EPS,
      POSTCODE_YEAR_MONTH_ID_M2_PDS,
      POSTCODE_YEAR_MONTH_ID_P2_PDS
    ),
    SINGLE_LINE_ADDRESS = coalesce(
      SINGLE_LINE_ADDRESS_YEAR_MONTH_ID_EPS,
      SINGLE_LINE_ADDRESS_YEAR_MONTH_ID_PDS,
      SINGLE_LINE_ADDRESS_YEAR_MONTH_ID_M1_EPS,
      SINGLE_LINE_ADDRESS_YEAR_MONTH_ID_P1_EPS,
      SINGLE_LINE_ADDRESS_YEAR_MONTH_ID_M1_PDS,
      SINGLE_LINE_ADDRESS_YEAR_MONTH_ID_P1_PDS,
      SINGLE_LINE_ADDRESS_YEAR_MONTH_ID_M2_EPS,
      SINGLE_LINE_ADDRESS_YEAR_MONTH_ID_P2_EPS,
      SINGLE_LINE_ADDRESS_YEAR_MONTH_ID_M2_PDS,
      SINGLE_LINE_ADDRESS_YEAR_MONTH_ID_P2_PDS
    )
  ) %>%
  select(YEAR_MONTH, NHS_NO, POSTCODE, SINGLE_LINE_ADDRESS)

# Combine EPS and paper data with the FACT

# Stack EPS and paper back together
fact_db <- union_all(
  x = eps_fact_db %>%
    # Remember to filter unwanted periods from the EPS FACT table (as it 
    # includes the buffer used to find addresses for paper forms)
    filter(
      CALC_AGE >= 65L,
      YEAR_MONTH >= 202004L & YEAR_MONTH <= 202103L
    ) %>%
    distinct(across(-c(YEAR_MONTH_ID, ITEM_COUNT))), 
  y = paper_fact_db %>%    
    distinct(across(-c(YEAR_MONTH_ID, ITEM_COUNT))) %>%
    # Join the addresses
    left_join(y = paper_patient_db)
)

# Write the table back to DALP with indexes
fact_db %>%
  compute(
    name = "INT615_FORM_LEVEL_FACT",
    indexes = list(c("YEAR_MONTH", "PF_ID"), c("POSTCODE")),
    temporary = FALSE
  )

# Disconnect from database
DBI::dbDisconnect(con_dalp)
