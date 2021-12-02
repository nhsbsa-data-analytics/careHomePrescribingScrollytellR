# Load library
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Load lazy tables for script 

# 1. AddressBase base table
ab_base <- con %>%
  tbl(from = in_schema("ADNSH", "ADDRESSBASE_PLUS_CARE_HOME"))

# 2. Prescription base table
presc_base <- con %>%
  tbl(from = in_schema("ADNSH", "FORM_LEVEL_CARE_HOME_FACT"))

# Format primary_df for function input
presc_base <- presc_base %>%
  select(ADDRESS_RECORD_ID, PAT_POSTCODE, PAT_ADDRESS) %>%
  distinct()

# Get Match Results
matches <- calc_match_addresses(
  primary_df = presc_base,
  primary_postcode_col = "PAT_POSTCODE",
  primary_address_col = "PAT_ADDRESS",
  lookup_df = ab_base,
  lookup_postcode_col = "AB_POSTCODE",
  lookup_address_col = "AB_ADDRESS"
)

# Write the table back to the DB
matches %>%
  nhsbsaR::oracle_create_table(table_name = "CARE_HOME_MATCH")

# Disconnect from database
DBI::dbDisconnect(con)

#-------------------------------------------------------------------------------