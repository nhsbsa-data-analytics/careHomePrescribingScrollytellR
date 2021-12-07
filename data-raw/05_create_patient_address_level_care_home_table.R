library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Check if the table exists
exists <- DBI::dbExistsTable(
  conn = con, 
  name = "INT615_ADDRESS_MATCHED_CARE_HOME"
)

# Drop any existing table beforehand
if (exists) {
  DBI::dbRemoveTable(conn = con, name = "INT615_ADDRESS_MATCHED_CARE_HOME")
}

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

# Match patient addresses to the AddressBase Plus and CQC care home addresses

# Get the distinct postcode and address combinations from the patient data along
# with some attributes
patient_address_db <- form_fact_db %>%
  # If the address is NA we don't want to consider it
  filter(!is.na(SINGLE_LINE_ADDRESS)) %>%
  # Add monthly patient count
  group_by(YEAR_MONTH, POSTCODE, SINGLE_LINE_ADDRESS) %>%
  mutate(
    PATIENT_COUNT = n_distinct(
      ifelse(IDENTIFIED_PATIENT == "Y", NHS_NO, NA_integer)
    )
  ) %>%
  ungroup() %>%
  # Add yearly attributes to the addresses
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  summarise(
    MONTHS_5PLUS_PATIENTS = n_distinct(
      ifelse(PATIENT_COUNT >= 5L, YEAR_MONTH, NA_integer_)
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
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, "ABOVE|CARAVAN|CHILDREN|HOLIDAY|MOBILE|NO FIXED ABODE|RESORT") > 0L,
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
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, "CARE HOME|CARE-HOME|NURSING HOME|NURSING-HOME|RESIDENTIAL HOME|RESIDENTIAL-HOME|REST HOME|REST-HOME") > 0L &
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, "ABOVE|CARAVAN|CHILDREN|HOLIDAY|MOBILE|NO FIXED ABODE|RESORT") == 0L &
        # Slightly stricter here
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, "CONVENT|HOSPITAL|RESORT|MARINA|MONASTERY|RECOVERY") == 0L,
      yes = "KEY WORD",
      no = NA_character_
    ),
    CH_FLAG = ifelse(MATCH_TYPE == "KEY WORD", 1L, CH_FLAG)
  )

# Get postcodes where there is a care home
care_home_postcodes_db <- addressbase_plus_db %>%
  filter(CH_FLAG == 1L) %>%
  distinct(POSTCODE)

# Manually override the care home flag for non care home patient addresses that 
# have 5 or more patients in a single month that are in a care home postcode
patient_address_match_db <- patient_address_match_db %>%
  left_join(
    y = care_home_postcodes_db %>% 
      mutate(CH_POSTCODE = 1L),
    copy = TRUE
  ) %>%
  mutate(
    MATCH_TYPE = ifelse(
      test = 
        CH_FLAG == 0L & 
        CH_POSTCODE == 1L &
        MAX_MONTHLY_PATIENTS >= 5L, #& MONTHS_5PLUS_PATIENTS >= 3
      yes = "PATIENT COUNT",
      no = NA_character_
    ),
    CH_FLAG = ifelse(MATCH_TYPE == "PATIENT COUNT", 1L, CH_FLAG)
  ) %>%
  select(-CH_POSTCODE)

# Fill the care home flag and match type columns
patient_address_match_db <- patient_address_match_db %>%
  tidyr::replace_na(list(CH_FLAG = 0, MATCH_TYPE = "NO MATCH"))

# Write the table back to the DB
patient_address_match_db %>%
  nhsbsaR::oracle_create_table(
    table_name = "INT615_ADDRESS_MATCHED_CARE_HOME"
  )

# Disconnect from database
DBI::dbDisconnect(con)