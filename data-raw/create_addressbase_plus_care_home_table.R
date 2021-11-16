# Library
library(dplyr)
library(dbplyr)

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Check if the table exists
exists <- DBI::dbExistsTable(conn = con, name = "ADDRESSBASE_PLUS_CARE_HOME")

# Drop any existing table beforehand
if (exists) DBI::dbRemoveTable(conn = con, name = "ADDRESSBASE_PLUS_CARE_HOME")

# Initial Lazy Tables from raw data

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
    RELEASE_DATE == TO_DATE("2021-03-15", "YYYY:MM:DD")
  ) %>%
  mutate(CH_FLAG = ifelse(CLASS == "RI01", 1, 0))

# Get postcodes where there is a care home present. We use POSTCODE_LOCATOR as
# it is equal to POSTCODE (whenever one exists) but more complete
care_home_postcodes_db <- addressbase_plus_db %>%
  filter(CH_FLAG == 1) %>%
  distinct(POSTCODE_LOCATOR)

# Filter AddressBase Plus to postcodes where there is a care home present
addressbase_plus_db <- addressbase_plus_db %>%
  inner_join(y = care_home_postcodes_db)

# Create a long table of single line addresses
addressbase_plus_db <- addressbase_plus_db %>%
  addressMatchR::calc_addressbase_plus_dpa_single_line_address() %>%
  addressMatchR::calc_addressbase_plus_geo_single_line_address() %>%
  select(
    UPRN,
    CH_FLAG,
    DPA_POSTCODE = POSTCODE,
    DPA_SINGLE_LINE_ADDRESS,
    GEO_POSTCODE = POSTCODE_LOCATOR,
    GEO_SINGLE_LINE_ADDRESS
  ) %>%
  tidyr::pivot_longer(
    cols = -c(UPRN, CH_FLAG),
    names_to = c("ADDRESS_TYPE", ".value"),
    names_sep = "_" # Should use names_pattern but can't get it to work
  ) %>%
  # Hack the names_sep result back to what it should be (ignore warning as
  # splits {DPA,GEO}_SINGLE_LINE_ADDRESS into {DPA,GEO} / SINGLE / LINE /
  # ADDRESS and drops LINE / ADDRESS as no data exists)
  rename(SINGLE_LINE_ADDRESS = SINGLE) %>%
  # Remove NA POSTCODE / SINGLE_LINE_ADDRESS (these are DPA)
  filter(!is.na(POSTCODE) & !is.na(SINGLE_LINE_ADDRESS))

# Tidy postcode and format single line addresses for tokenisation
addressbase_plus_db <- addressbase_plus_db %>%
  addressMatchR::tidy_postcode(col = POSTCODE) %>%
  addressMatchR::tidy_single_line_address(col = SINGLE_LINE_ADDRESS)

# Combine rows where there DPA and GEO single line addresses are equal into BOTH
addressbase_plus_db <- addressbase_plus_db %>%
  mutate(
    DPA = ifelse(ADDRESS_TYPE == "DPA", 1, 0),
    GEO = ifelse(ADDRESS_TYPE == "GEO", 1, 0)
  ) %>%
  group_by(across(c(-ADDRESS_TYPE, -DPA, -GEO))) %>%
  summarise(DPA = sum(DPA), GEO = sum(GEO), .groups = "drop") %>%
  mutate(
    ADDRESS_TYPE = case_when(
      DPA == 1 & GEO == 1 ~ "BOTH",
      DPA == 1 & GEO == 0 ~ "DPA",
      DPA == 0 & GEO == 1 ~ "GEO",
      TRUE ~ "ERROR"
    )
  ) %>%
  select(-DPA, -GEO) %>%
  relocate(ADDRESS_TYPE, .after = CH_FLAG)

# Pull the UPRNs that have two different addresses
combined_addresses_db <- addressbase_plus_db %>%
  group_by(UPRN) %>%
  summarise(NO_ADDRESS_TYPE = n_distinct(ADDRESS_TYPE)) %>%
  ungroup() %>%
  filter(NO_ADDRESS_TYPE > 1) %>%
  select(UPRN) %>%
  inner_join(y = addressbase_plus_db)

# Create a combined single line address for these of the form "{DPA} / {GEO}"
combined_addresses_db <- combined_addresses_db %>%
  tidyr::pivot_wider(
    id_cols = c(UPRN, CH_FLAG, POSTCODE),
    names_from = ADDRESS_TYPE,
    values_from = SINGLE_LINE_ADDRESS
  ) %>%
  mutate(SINGLE_LINE_ADDRESS = paste0(DPA, " / ", GEO)) %>%
  select(-DPA, -GEO)

# Compute query and save in temporary remote table
combined_addresses_db %>%
  nhsbsaR::oracle_create_table(table_name = "TMP_COMBINED_ADDRESSES")

# Pull the temporary remote table
combined_addresses_db <- tbl(
  src = con,
  from = sql("SELECT * FROM TMP_COMBINED_ADDRESSES")
)

# Tokenise the single line addresses and add token type ("D" = digit,
# "C" = character)
addressbase_plus_db <- addressbase_plus_db %>%
  nhsbsaR::oracle_unnest_tokens(col = "SINGLE_LINE_ADDRESS", drop = FALSE) %>%
  mutate(TOKEN_TYPE = ifelse(REGEXP_LIKE(TOKEN, "[0-9]"), "D", "C"))

# Compute query and save in temporary remote table
addressbase_plus_db %>%
  nhsbsaR::oracle_create_table(table_name = "TMP_ADDRESSBASE_PLUS")

# Pull the temporary remote table
addressbase_plus_db <- tbl(
  src = con,
  from = sql("SELECT * FROM TMP_ADDRESSBASE_PLUS")
)

# Where DPA and GEO have different single line addresses for the same UPRN, get
# a superset of all tokens
combined_addresses_db <- combined_addresses_db %>%
  inner_join(
    y = addressbase_plus_db %>% 
      select(UPRN, TOKEN, TOKEN_TYPE)
  ) %>%
  distinct()

# Indicate where each token has originated
combined_addresses_db <- combined_addresses_db%>%
  left_join(
    y = addressbase_plus_db %>%
      filter(ADDRESS_TYPE == "DPA") %>%
      mutate(DPA_TOKEN = 1) %>%
      select(-ADDRESS_TYPE, -SINGLE_LINE_ADDRESS, -TOKEN_NUMBER)
  ) %>%
  left_join(
    y = addressbase_plus_db %>%
      filter(ADDRESS_TYPE == "GEO") %>%
      mutate(GEO_TOKEN = 1) %>%
      select(-ADDRESS_TYPE, -SINGLE_LINE_ADDRESS, -TOKEN_NUMBER)
  )

# Filter combined addresses where the one DPA / GEO address is a subset of the 
# other
combined_addresses_db <- combined_addresses_db %>%
  inner_join(
    y = combined_addresses_db %>%
      group_by(UPRN) %>%
      summarise(
        MISSING_DPA_TOKENS = sum(ifelse(is.na(DPA_TOKEN), 1, 0)),
        MISSING_GEO_TOKENS = sum(ifelse(is.na(GEO_TOKEN), 1, 0))
      ) %>%
      # If one address is missing 0 tokens then we don't need a combined address
      filter(MISSING_DPA_TOKENS > 0 & MISSING_GEO_TOKENS > 0) %>%
      select(UPRN)
  ) %>%
  select(-DPA_TOKEN, -GEO_TOKEN)

# Add the combined token rows to our data
addressbase_plus_db <- addressbase_plus_db %>%
  union_all(
    y = combined_addresses_db %>%
      mutate(ADDRESS_TYPE = "COMBINED")
  )

# Pull non unique postcode and single line address combinations
non_unique_postcode_address_combinations_db <- addressbase_plus_db %>%
  distinct(UPRN, POSTCODE, SINGLE_LINE_ADDRESS) %>%
  count(POSTCODE, SINGLE_LINE_ADDRESS) %>% 
  filter(n > 1) %>%
  select(-n)

# Add more of the address information
non_unique_postcode_address_combinations_db <- addressbase_plus_db %>%
  select(-UPRN, TOKEN_NUMBER, -TOKEN, -TOKEN_NUMBER, -TOKEN_TYPE) %>%
  distinct() %>%
  inner_join(y = non_unique_postcode_address_combinations_db)

# Update the care home flag on the non unique postcode and single line address
# combinations. See if they have differing care home flags, if so allocate them 
# all NA
non_unique_postcode_address_combinations_care_home_flag_db <- 
  non_unique_postcode_address_combinations_db %>%
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  summarise(
    CH_FLAG_0 = sum(ifelse(CH_FLAG == 0, 1, 0)),
    CH_FLAG_1 = sum(ifelse(CH_FLAG == 1, 1, 0)),
  ) %>%
  ungroup() %>%
  mutate(
    CH_FLAG = case_when(
      CH_FLAG_0 > 0 & CH_FLAG_1 == 0 ~ 0,
      CH_FLAG_1 > 0 & CH_FLAG_0 == 0 ~ 1,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(CH_FLAG, POSTCODE, SINGLE_LINE_ADDRESS)

# Update the address type on the non unique postcode and single line address
# combinations. See if they have differing address types, if so allocate them 
# one based on priority: BOTH > DPA or GEO > NA.
# NOTE: There are no COMBINED address types here
non_unique_postcode_address_combinations_address_type_db <- 
  non_unique_postcode_address_combinations_db %>%
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  summarise(
    DPA = sum(ifelse(ADDRESS_TYPE == "DPA", 1, 0)),
    GEO = sum(ifelse(ADDRESS_TYPE == "GEO", 1, 0)),
    BOTH = sum(ifelse(ADDRESS_TYPE == "BOTH", 1, 0))
  ) %>%
  ungroup() %>%
  mutate(
    ADDRESS_TYPE = case_when(
      BOTH > 0 ~ "BOTH",
      BOTH == 0 & DPA > 0 & GEO == 0 ~ "DPA",
      BOTH == 0 & GEO > 0 & DPA == 0 ~ "GEO",
      TRUE ~ NA_character_
    )
  ) %>%
  select(ADDRESS_TYPE, POSTCODE, SINGLE_LINE_ADDRESS)
  
# Add the updated care home flag and address type for the non unique postcode
# and single line address combinations
non_unique_postcode_address_combinations_db <- 
  non_unique_postcode_address_combinations_care_home_flag_db %>%
  inner_join(
    y = non_unique_postcode_address_combinations_address_type_db
  ) %>%
  # Mock NA UPRN as we can't ever choose between 2
  mutate(UPRN = NA_integer_) %>%
  select(UPRN, CH_FLAG, ADDRESS_TYPE, POSTCODE, SINGLE_LINE_ADDRESS)

# Add the token information 
non_unique_postcode_address_combinations_db <- 
  non_unique_postcode_address_combinations_db %>%
  inner_join(
    y = addressbase_plus_db %>%
    distinct(SINGLE_LINE_ADDRESS, TOKEN_NUMBER, TOKEN, TOKEN_TYPE)
  )

# Update the duplicate address rows in the AddressBase Plus care home table
addressbase_plus_db  <- addressbase_plus_db  %>%
  anti_join(
    y = non_unique_postcode_address_combinations_db %>%
      distinct(POSTCODE, SINGLE_LINE_ADDRESS)
  ) %>%
  union_all(y = non_unique_postcode_address_combinations_db)

# Write the table back to the DB
addressbase_plus_db %>%
  nhsbsaR::oracle_create_table(table_name = "ADDRESSBASE_PLUS_CARE_HOME")

# Drop the temporary tables
DBI::dbRemoveTable(conn = con, name = "TMP_COMBINED_ADDRESSES")
DBI::dbRemoveTable(conn = con, name = "TMP_ADDRESSBASE_PLUS")

# Disconnect from database
DBI::dbDisconnect(con)
