library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Check if the table exists
exists <- DBI::dbExistsTable(
  conn = con, 
  name = "INT615_ADDRESS_MATCHED"
)

# Drop any existing table beforehand
if (exists) {
  DBI::dbRemoveTable(conn = con, name = "INT615_ADDRESS_MATCHED")
}

# Initial lazy tables from database

# Create a lazy table from the item level FACT table
fact_db <- con %>%
  tbl(from = "INT615_FORM_LEVEL_FACT")

# Create a lazy table from the AddressBase Plus and CQC care home table
addressbase_plus_cqc_db <- con %>%
  tbl(from = "INT615_ADDRESSBASE_PLUS_CQC")

# Match patient addresses to the AddressBase Plus and CQC care home addresses

# Get the distinct postcode and address combinations from the patient data along
# with some attributes
patient_address_db <- fact_db %>%
  # If the address is NA we don't want to consider it
  filter(!is.na(SINGLE_LINE_ADDRESS)) %>%
  # Add monthly patient count
  group_by(YEAR_MONTH, POSTCODE, SINGLE_LINE_ADDRESS) %>%
  mutate(
    TOTAL_MONTHLY_PATIENTS = n_distinct(
      ifelse(PATIENT_IDENTIFIED == "Y", NHS_NO, NA_integer_)
    )
  ) %>%
  # Add yearly attributes to the addresses
  ungroup(YEAR_MONTH) %>%
  summarise(
    TOTAL_FORMS = n(),
    TOTAL_PATIENTS = n_distinct(
      ifelse(PATIENT_IDENTIFIED == "Y", NHS_NO, NA_integer_)
    ),
    MONTHS_5PLUS_PATIENTS = n_distinct(
      ifelse(TOTAL_MONTHLY_PATIENTS >= 5L, YEAR_MONTH, NA_integer_)
    ),
    MAX_MONTHLY_PATIENTS = max(TOTAL_MONTHLY_PATIENTS, na.rm = TRUE)
  ) %>%
  ungroup()

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

# Move SINGLE_LINE_ADDRESS_LOOKUP to after SINGLE_LINE_ADDRESS
match_db <- match_db %>%
  relocate(SINGLE_LINE_ADDRESS_LOOKUP, .after = SINGLE_LINE_ADDRESS)

# Manually override the care home flag (and nursing / residential flags) for 
# matched care home patient addresses that contain anything to strongly suggest 
# the property is not a care home for the elderly
match_db <- match_db %>%
  mutate(
    CH_FLAG = ifelse(
      test = 
        CH_FLAG == 1L & 
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, "ABOVE|CARAVAN|CHILDREN|HOLIDAY|MOBILE|NO FIXED ABODE|RESORT") > 0L,
      yes = 0L,
      no = CH_FLAG
    ),
    NURSING_HOME_FLAG = ifelse(
      test = CH_FLAG == 0L, 
      yes = NA_integer_, 
      no = NURSING_HOME_FLAG
    ),
    RESIDENTIAL_HOME_FLAG = ifelse(
      test = CH_FLAG == 0L, 
      yes = NA_integer_, 
      no = RESIDENTIAL_HOME_FLAG
    )
  )

# Join the matches back to the patient addresses
patient_address_match_db <- patient_address_db %>%
  left_join(
    y = match_db,
    copy = TRUE
  )

# Fill the missing care home flag and match type columns
patient_address_match_db <- patient_address_match_db %>%
  tidyr::replace_na(list(CH_FLAG = 0L, MATCH_TYPE = "NO MATCH"))

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
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, "CONVENT|HOSPITAL|MARINA|MONASTERY|RECOVERY|RESORT") == 0L,
      yes = "KEY WORD",
      no = MATCH_TYPE
    ),
    CH_FLAG = ifelse(MATCH_TYPE == "KEY WORD", 1L, CH_FLAG)
  )

# Get postcodes where there is a care home
care_home_postcodes_db <- addressbase_plus_cqc_db %>%
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
        !is.na(CH_POSTCODE) &
        MAX_MONTHLY_PATIENTS >= 5L, #& MONTHS_5PLUS_PATIENTS >= 3
      yes = "PATIENT COUNT",
      no = MATCH_TYPE
    ),
    CH_FLAG = ifelse(MATCH_TYPE == "PATIENT COUNT", 1L, CH_FLAG)
  ) %>%
  select(-CH_POSTCODE)

# Write the table back to the DB
patient_address_match_db %>%
  nhsbsaR::oracle_create_table(
    table_name = "INT615_ADDRESS_MATCHED"
  )

# Disconnect from database
DBI::dbDisconnect(con)