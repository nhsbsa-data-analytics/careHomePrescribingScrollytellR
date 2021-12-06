library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Initial lazy tables from database

# Create a lazy table from the year month table
year_month_db <- con %>%
  tbl(from = in_schema("DALL_REF", "YEAR_MONTH_DIM"))

# Create a lazy table from the item level FACT table
item_fact_db <- con %>%
  tbl(from = in_schema("SB_AML", "PX_FORM_ITEM_ELEM_COMB_FACT"))

# Create a lazy table from the AddressBase Plus and CQC care home table
addressbase_plus_cqc_db <- con %>%
  tbl(from = "INT615_ADDRESSBASE_PLUS_CQC_CARE_HOME")

# Create a lazy table from the form level care home FACT table
form_fact_db <- con %>%
  tbl(from = "INT615_FORM_LEVEL_FACT_CARE_HOME")

# Match patient addresses to the AddressBase Plus and CQC care home addresses

# Get the distinct postcode and address combinations from the patient data along
# with some attributes
patient_address_db <- form_fact_db %>%
  # If the address is NA we don't want to consider it
  filter(!is.na(SINGLE_LINE_ADDRESS)) %>%
  # Add monthly patient count
  group_by(YEAR_MONTH, POSTCODE, SINGLE_LINE_ADDRESS) %>%
  mutate(PATIENT_COUNT = n_distinct(NHS_NO)) %>%
  ungroup() %>%
  # Add yearly attributes to the addresses
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  summarise(
    MONTHS_5PLUS_PATIENTS = sum(
      ifelse(PATIENT_COUNT >= 5L, 1L, 0L),
      na.rm = TRUE
    ),
    MAX_MONTHLY_PATIENTS = max(PATIENT_COUNT, na.rm = TRUE)
  )

# Match the patients address to the AddressBase Plus and CQC care home addresses
match_db <- addressMatchR::calc_match_addresses(
  primary_df = patient_address_db,
  primary_postcode_col = "POSTCODE",
  primary_address_col = "SINGLE_LINE_ADDRESS",
  lookup_df = addressbase_plus_cqc_db,
  lookup_postcode_col = "POSTCODE",
  lookup_address_col = "SINGLE_LINE_ADDRESS"
)

# At this point it is possible that some of the Jaro-Winkler matches are tied, 
# so we prioritise the best match by selecting a non care home property first 
# (to err on the side of caution) if one exists, otherwise pick any
match_db <- match_db %>%
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  slice_min(
    order_by = CH_FLAG, 
    with_ties = FALSE
  ) %>%
  ungroup()

# Manually override the care home flag (and nursing / residential flags) for 
# matched care home patient addresses that contain anything to strongly suggest 
# the property is not a care home for the elderly
match_db <- match_db %>%
  mutate(
    MATCH_TYPE = ifelse(
      test = 
        CH_FLAG == 1L & 
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, "ABOVE|CARAVAN|CHILDREN|HOLIDAY|MOBILE|NO FIXED ABODE|RESORT") > 0,
      yes = 0L,
      no = 1L
    ),
    NURSING_HOME_FLAG = ifelse(CH_FLAG == 0L, NA_integer_, 1L),
    RESIDENTIAL_HOME_FLAG = ifelse(CH_FLAG == 0L, NA_integer_, 1L)
  )

# Join the matches back to the patient addresses
patient_address_match_db <- patient_address_db %>%
  left_join(y = match_db)

# Manually override the care home flag for non care home patient addresses that 
# contain anything to strongly suggest it is a care home for the elderly
patient_address_match_db <- patient_address_match_db %>%
  mutate(
    MATCH_TYPE = ifelse(
      test = 
        CH_FLAG == 0L & 
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, "CARE HOME|CARE-HOME|NURSING HOME|NURSING-HOME|RESIDENTIAL HOME|RESIDENTIAL-HOME|REST HOME|REST-HOME") > 0 &
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, "ABOVE|CARAVAN|CHILDREN|HOLIDAY|MOBILE|NO FIXED ABODE|RESORT") == 0 &
        # Slightly stricter here
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, "CONVENT|HOSPITAL|RESORT|MARINA|MONASTERY|RECOVERY") == 0,
      yes = "KEY WORD",
      no = NA_character_
    ),
    CH_FLAG = ifelse(MATCH_TYPE == "KEY WORD", 1L, CH_FLAG)
  )

# Manually override the care home flag for non care home patient addresses that 
# have 5 or more patients in a single month
patient_address_match_db <- patient_address_match_db %>%
  mutate(
    MATCH_TYPE = ifelse(
      test = 
        CH_FLAG == 0L & 
        MAX_MONTHLY_PATIENTS >= 5L, #& MONTHS_5PLUS_PATIENTS >= 3
      yes = "PATIENT COUNT",
      no = NA_character_
    ),
    CH_FLAG = ifelse(MATCH_TYPE == "PATIENT COUNT", 1L, CH_FLAG)
  )

# Create item level FACT table

# Filter to 2020/2021
year_month_db <- year_month_db %>%
  filter(FINANCIAL_YEAR == "2020/2021") %>%
  select(YEAR_MONTH)

# Filter to elderly patients in 2020/2021 and required columns
item_fact_db <- item_fact_db %>%
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
  select(
    YEAR_MONTH,
    PART_DATE = EPS_PART_DATE,
    EPM_ID,
    PF_ID,
    NHS_NO,
    EPS_FLAG,
    CALC_PREC_DRUG_RECORD_ID,
    ITEM_COUNT,
    ITEM_PAY_DR_NIC,
    ITEM_CALC_PAY_QTY
  ) %>%
  inner_join(
    y = year_month_db,
    copy = TRUE
  )

# Join the postcode and addresses
item_fact_db <- item_fact_db %>%
  left_join(
    y = form_fact_db,
    copy = TRUE
  )

# Now we join the columns of interest back to the fact table and fill the 
# care home flag and match type columns
item_fact_match_db <- item_fact_db %>%
  left_join(
    y = patient_address_match_db,
    copy = TRUE
  ) %>%
  tidyr::replace_na(list(CH_FLAG = 0, MATCH_TYPE = "NO MATCH"))

# Write the table back to the DB
item_fact_match_db %>%
  nhsbsaR::oracle_create_table(
    table_name = "INT615_ITEM_LEVEL_FACT_MATCHED_CARE_HOME"
  )

# Disconnect from database
DBI::dbDisconnect(con)