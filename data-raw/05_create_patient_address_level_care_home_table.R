library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Match patient addresses to the AddressBase Plus and CQC care home addresses

# Create a lazy table from the AddressBase Plus and CQC care home table
addressbase_plus_cqc_db <- con %>%
  tbl(from = "INT615_ADDRESSBASE_PLUS_CQC_CARE_HOME")

# Create a lazy table from the from level care home FACT table
fact_db <- con %>%
  tbl(from = "INT615_FORM_LEVEL_FACT_CARE_HOME")

# Get the distinct postcode and address combinations from the patient data along
# with some attributes
patient_address_db <- fact_db %>%
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
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, "CHILDREN|MOBILE|ABOVE|CARAVAN|RESORT|HOLIDAY|NO FIXED ABODE") == 1,
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
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, "NURSING HOME|NURSING-HOME|RESIDENTIAL HOME|RESIDENTIAL-HOME|REST HOME|REST-HOME|CARE HOME|CARE-HOME") > 0 &
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, "CHILDREN|MOBILE|ABOVE|CARAVAN|RESORT|CONVENT|MONASTERY|HOLIDAY|MARINA|RECOVERY|HOSPITAL|NO FIXED ABODE") == 0,
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
        MAX_MONTHLY_PATIENTS >= 5, #& MONTHS_5PLUS_PATIENTS >= 3
      yes = "PATIENT COUNT",
      no = NA_character_
    ),
    CH_FLAG = ifelse(MATCH_TYPE == "PATIENT COUNT", 1L, CH_FLAG)
  )

# Now we join the columns of interest back to the fact table and fill the 
# care home flag and match type columns
fact_match_db <- fact_db %>%
  left_join(y = patient_address_match_db) %>%
  tidyr::replace_na(list(CH_FLAG = 0, MATCH_TYPE = "NO MATCH"))