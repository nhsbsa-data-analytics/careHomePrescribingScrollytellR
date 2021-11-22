# Librrary
library(dplyr)
library(dbplyr)

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Check if the table exists
exists <- DBI::dbExistsTable(conn = con, name = "ADDRESSBASE_PLUS_CARE_HOME")

# Drop any existing table beforehand
if (exists) {
  DBI::dbRemoveTable(conn = con, name = "ADDRESSBASE_PLUS_CARE_HOME")
}

# Part One: AdddressBase plus base table ---------------------------------------

# Create a lazy table from the AddressBase Plus table
addressbase_plus_db <- con %>% 
  tbl(from = in_schema("DALL_REF", "ADDRESSBASE_PLUS"))

# Filter AddressBase Plus to English properties in at the end of 2021 FY and
# create a care home flag
addressbase_plus_db <- addressbase_plus_db %>%
  dplyr::filter(
    COUNTRY == "E",
    substr(CLASS, 1, 1) != "L", # Land
    substr(CLASS, 1, 1) != "O", # Other (Ordnance Survey only)
    substr(CLASS, 1, 2) != "PS", # Street Record
    substr(CLASS, 1, 2) != "RC", # Car Park Space
    substr(CLASS, 1, 2) != "RG", # Lock-Up / Garage / Garage Court
    substr(CLASS, 1, 1) != "Z", # Object of interest
    RELEASE_DATE == to_date("2021-03-15", "YYYY:MM:DD")
  ) %>%
  dplyr::mutate(CH_FLAG = ifelse(CLASS == "RI01", 1, 0))

# Get postcodes where there is a care home present. We use POSTCODE_LOCATOR as
# it is equal to POSTCODE (whenever one exists) but more complete
care_home_postcodes_db <- addressbase_plus_db %>%
  dplyr::filter(CH_FLAG == 1) %>%
  dplyr::distinct(POSTCODE_LOCATOR)

# Filter AddressBase Plus to postcodes where there is a care home present
addressbase_plus_db <- addressbase_plus_db %>%
  dplyr::inner_join(y = care_home_postcodes_db)

# Create a Wide table of 3 single line address types
addressbase_plus_db <- addressbase_plus_db %>%
  addressMatchR::calc_addressbase_plus_dpa_single_line_address() %>%
  addressMatchR::calc_addressbase_plus_geo_single_line_address() %>%
  dplyr::select(
    UPRN,
    CLASS,
    CH_FLAG,
    AB_POSTCODE = POSTCODE_LOCATOR,
    DPA_SINGLE_LINE_ADDRESS,
    GEO_SINGLE_LINE_ADDRESS
  ) %>% 
  addressMatchR::tidy_postcode(col = AB_POSTCODE) %>% 
  addressMatchR::tidy_single_line_address(col = DPA_SINGLE_LINE_ADDRESS) %>% 
  addressMatchR::tidy_single_line_address(col = GEO_SINGLE_LINE_ADDRESS) %>% 
  merge_address_strings(
    col_one = DPA_SINGLE_LINE_ADDRESS,
    col_two = GEO_SINGLE_LINE_ADDRESS
  ) %>% 
  rename(CORE_SINGLE_LINE_ADDRESS = MERGE_STRING)

# Convert to a long table of distinct stacked single line addresses
addressbase_plus_db <- addressbase_plus_db %>%
  tidyr::pivot_longer(
    cols = -c(UPRN, CLASS, CH_FLAG, AB_POSTCODE),
    names_to = "ADDRESS_TYPE",
    values_to = "AB_ADDRESS"
  ) %>% 
  filter(!is.na(AB_ADDRESS)) %>% 
  select(UPRN, CLASS, CH_FLAG, AB_POSTCODE, AB_ADDRESS) %>% 
  distinct() %>%
  group_by(AB_POSTCODE, AB_ADDRESS) %>% 
  mutate(UPRN_COUNT = dense_rank(UPRN)) %>% 
  ungroup() %>% 
  filter(UPRN_COUNT == 1) %>% 
  select(-UPRN_COUNT) %>% 
  arrange(UPRN) %>% 
  mutate(UPRN_ID = row_number())

# Write the table back to the DB
addressbase_plus_db %>%
  nhsbsaR::oracle_create_table(table_name = "ADDRESSBASE_PLUS_CARE_HOME")

# Disconnect from database
DBI::dbDisconnect(con)

#-------------------------------------------------------------------------------
