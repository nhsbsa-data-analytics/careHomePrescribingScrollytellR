library(dplyr)
library(dbplyr)

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Check if the table exists
exists <- DBI::dbExistsTable(conn = con, name = "INT615_ADDRESSBASE_PLUS_CQC")

# Drop any existing table beforehand
if (exists) {
  DBI::dbRemoveTable(conn = con, name = "INT615_ADDRESSBASE_PLUS_CQC")
}

# Process CQC care home table

# Create a lazy table from the CQC care home table
cqc_db <- con %>% 
  tbl(from = "INT615_CQC")

# Convert registration and deregistration columns to dates and filter to 2020/21
cqc_db <- cqc_db %>%
  mutate(
    REGISTRATION_DATE = ifelse(
      test = is.na(REGISTRATION_DATE),
      yes = NA,
      no = TO_DATE(REGISTRATION_DATE ,"YYYY-MM-DD")
    ),
    DEREGISTRATION_DATE = ifelse(
      test = is.na(DEREGISTRATION_DATE),
      yes = NA,
      no = TO_DATE(DEREGISTRATION_DATE ,"YYYY-MM-DD")
    )
  ) %>% 
  filter(
    REGISTRATION_DATE <= TO_DATE("2021-03-31","YYYY-MM-DD"),
    is.na(DEREGISTRATION_DATE) | 
      DEREGISTRATION_DATE >= TO_DATE("2020-04-01","YYYY-MM-DD")
  )

# Create a tidy single line address and postcode
cqc_db <- cqc_db %>%
  mutate(
    SINGLE_LINE_ADDRESS = paste(
      NAME,
      POSTAL_ADDRESS_LINE_1,
      POSTAL_ADDRESS_LINE_2,
      POSTAL_ADDRESS_TOWN_CITY,
      POSTAL_ADDRESS_COUNTY
    )
  ) %>%
  addressMatchR::tidy_single_line_address(col = SINGLE_LINE_ADDRESS) %>%
  addressMatchR::tidy_postcode(col = POSTAL_CODE) %>%
  rename(POSTCODE = POSTAL_CODE)

# Convert to a distinct postcode and single line address table by taking the 
# max of the attribute columns
cqc_uprn_postcode_address_db <- cqc_db %>%
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  summarise(
    LOCATION_ID = max(LOCATION_ID, na.rm = TRUE),
    UPRN = max(as.numeric(UPRN), na.rm = TRUE), # change to numeric
    NURSING_HOME_FLAG = max(as.integer(NURSING_HOME), na.rm = TRUE),
    RESIDENTIAL_HOME_FLAG = max(as.integer(RESIDENTIAL_HOME), na.rm = TRUE)
  ) %>% 
  ungroup() %>%
  relocate(UPRN, LOCATION_ID)

# Process AddressBase Plus care home table

# Create a lazy table from the AddressBase Plus table
addressbase_plus_db <- con %>% 
  tbl(from = in_schema("DALL_REF", "ADDRESSBASE_PLUS"))

# Filter AddressBase Plus to English properties in at the end of 2021 FY and
# create a care home flag
addressbase_plus_db <- addressbase_plus_db %>%
  filter(
    COUNTRY == "E",
    substr(CLASS, 1, 1) != "L", # Land
    substr(CLASS, 1, 1) != "O", # Other (Ordnance Survey only)
    substr(CLASS, 1, 2) != "PS", # Street Record
    substr(CLASS, 1, 2) != "RC", # Car Park Space
    substr(CLASS, 1, 2) != "RG", # Lock-Up / Garage / Garage Court
    substr(CLASS, 1, 1) != "Z", # Object of interest
    RELEASE_DATE == TO_DATE("2021-03-15", "YYYY-MM-DD")
  ) %>%
  mutate(CH_FLAG = ifelse(CLASS == "RI01", 1L, 0L)) %>%
  # Take POSTCODE_LOCATOR as the postcode as it is equal to POSTCODE (whenever 
  # one exists) but more complete and tidy it
  mutate(POSTCODE = POSTCODE_LOCATOR) %>%
  addressMatchR::tidy_postcode(col = POSTCODE)

# Get postcodes where there is a care home present (including CQC data)
care_home_postcodes_db <- 
  union_all(
    x = addressbase_plus_db %>% 
      filter(CH_FLAG == 1L) %>%
      select(POSTCODE),
    y = cqc_uprn_postcode_address_db %>%
      select(POSTCODE)
  )

# Filter AddressBase Plus to postcodes where there is a care home present
addressbase_plus_db <- addressbase_plus_db %>%
  semi_join(y = care_home_postcodes_db)

# Create a Wide table of 3 single line address types
addressbase_plus_db <- addressbase_plus_db %>%
  addressMatchR::calc_addressbase_plus_dpa_single_line_address() %>%
  addressMatchR::calc_addressbase_plus_geo_single_line_address() %>%
  addressMatchR::tidy_single_line_address(col = DPA_SINGLE_LINE_ADDRESS) %>% 
  addressMatchR::tidy_single_line_address(col = GEO_SINGLE_LINE_ADDRESS) %>% 
  select(
    UPRN,
    POSTCODE,
    DPA_SINGLE_LINE_ADDRESS,
    GEO_SINGLE_LINE_ADDRESS,
    CH_FLAG
  ) %>% 
  nhsbsaR::oracle_merge_strings(
    first_col = "DPA_SINGLE_LINE_ADDRESS",
    second_col = "GEO_SINGLE_LINE_ADDRESS",
    merge_col = "CORE_SINGLE_LINE_ADDRESS"
  )

# Combine AddressBase Plus (care home postcodes) and CQC

# Join the CQC attributes to existing UPRNs (take the max flags where there are 
# duplicate UPRN)
addressbase_plus_cqc_db <- addressbase_plus_db %>%
  left_join(
    y = cqc_uprn_postcode_address_db %>%
      group_by(UPRN) %>%
      summarise(
        LOCATION_ID = max(LOCATION_ID, na.rm = TRUE),
        NURSING_HOME_FLAG = max(NURSING_HOME_FLAG, na.rm = TRUE), 
        RESIDENTIAL_HOME_FLAG = max(RESIDENTIAL_HOME_FLAG, na.rm = TRUE)
      ) %>% 
      ungroup()
  )

# Convert to a long table of distinct stacked single line addresses
addressbase_plus_cqc_db <- addressbase_plus_cqc_db %>%
  tidyr::pivot_longer(
    cols = ends_with("SINGLE_LINE_ADDRESS"),
    names_to = "ADDRESS_TYPE",
    values_to = "SINGLE_LINE_ADDRESS"
  ) %>%
  filter(!is.na(SINGLE_LINE_ADDRESS)) %>%
  select(-ADDRESS_TYPE) %>%
  relocate(SINGLE_LINE_ADDRESS, .after = POSTCODE)

# Stack the CQC data and make distinct (take max row)
addressbase_plus_cqc_db <- addressbase_plus_cqc_db %>%
  union_all(y = cqc_uprn_postcode_address_db %>% mutate(CH_FLAG = 1L)) %>%
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>% 
  slice_max(order_by = UPRN, with_ties = FALSE) %>%
  ungroup()

# Write the table back to the DB with indexes
addressbase_plus_cqc_db %>%
  compute(
    name = "INT615_ADDRESSBASE_PLUS_CQC",
    indexes = list(c("UPRN", c("POSTCODE"))), # single line address too long
    temporary = FALSE
  )

# Disconnect from database
DBI::dbDisconnect(con)
