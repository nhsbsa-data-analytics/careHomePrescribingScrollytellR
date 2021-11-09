library(magrittr)

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Check if the table exists
exists <- DBI::dbExistsTable(conn = con, name = "ADDRESSBASE_PLUS_CARE_HOME")

# Drop any existing table beforehand
if (exists) {
  DBI::dbRemoveTable(conn = con, name = "ADDRESSBASE_PLUS_CARE_HOME")
}

# Create a lazy table from the AddressBase Plus table
addressbase_plus_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM DALL_REF.ADDRESSBASE_PLUS")
)

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

# Create a long table of single line addresses
addressbase_plus_db <- addressbase_plus_db %>%
  addressMatchR::calc_addressbase_plus_dpa_single_line_address() %>%
  addressMatchR::calc_addressbase_plus_geo_single_line_address() %>%
  dplyr::select(
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
  dplyr::rename(SINGLE_LINE_ADDRESS = SINGLE) %>%
  # Remove NA POSTCODE / SINGLE_LINE_ADDRESS (these are DPA)
  dplyr::filter(!is.na(POSTCODE) & !is.na(SINGLE_LINE_ADDRESS))

# Remove whitespace from postcodes and format single line addresses for
# tokenisation
addressbase_plus_db <- addressbase_plus_db %>%
  dplyr::mutate(POSTCODE = REGEXP_REPLACE(POSTCODE, " ", "")) %>%
  addressMatchR::tidy_single_line_address(col = SINGLE_LINE_ADDRESS)

# Combine rows where there DPA and GEO single line addresses are equal into BOTH
addressbase_plus_db <- addressbase_plus_db %>%
  dplyr::mutate(
    DPA = ifelse(ADDRESS_TYPE == "DPA", 1, 0),
    GEO = ifelse(ADDRESS_TYPE == "GEO", 1, 0)
  ) %>%
  dplyr::group_by(dplyr::across(c(-ADDRESS_TYPE, -DPA, -GEO))) %>%
  dplyr::summarise(DPA = sum(DPA), GEO = sum(GEO), .groups = "drop") %>%
  dplyr::mutate(
    ADDRESS_TYPE = dplyr::case_when(
      DPA == 1 & GEO == 1 ~ "BOTH",
      DPA == 1 & GEO == 0 ~ "DPA",
      DPA == 0 & GEO == 1 ~ "GEO",
      TRUE ~ "ERROR"
    )
  ) %>%
  dplyr::select(-DPA, -GEO) %>%
  dplyr::relocate(ADDRESS_TYPE, .after = CH_FLAG)

# Pull the UPRNs that have two different addresses
combined_addresses_db <- addressbase_plus_db %>%
  dplyr::group_by(UPRN) %>%
  dplyr::summarise(NO_ADDRESS_TYPE = dplyr::n_distinct(ADDRESS_TYPE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(NO_ADDRESS_TYPE > 1) %>%
  dplyr::select(UPRN) %>%
  dplyr::inner_join(addressbase_plus_db)

# Create a combined single line address for these of the form "{DPA} / {GEO}"
combined_addresses_db <- combined_addresses_db %>%
  tidyr::pivot_wider(
    id_cols = c(UPRN, CH_FLAG, POSTCODE),
    names_from = ADDRESS_TYPE,
    values_from = SINGLE_LINE_ADDRESS
  ) %>%
  dplyr::mutate(SINGLE_LINE_ADDRESS = paste0(DPA, " / ", GEO)) %>%
  dplyr::select(-DPA, -GEO)

# Compute query and save in temporary remote table
combined_addresses_db %>%
  nhsbsaR::oracle_create_table(table_name = "TMP_COMBINED_ADDRESSES")

# Pull the temporary remote table
combined_addresses_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM TMP_COMBINED_ADDRESSES")
)

# Tokenise the single line addresses and add token type ("D" = digit,
# "C" = character)
addressbase_plus_db <- addressbase_plus_db %>%
  nhsbsaR::oracle_unnest_tokens(col = "SINGLE_LINE_ADDRESS", drop = FALSE) %>%
  dplyr::mutate(TOKEN_TYPE = ifelse(REGEXP_LIKE(TOKEN, "[0-9]"), "D", "C"))

# Compute query and save in temporary remote table
addressbase_plus_db %>%
  nhsbsaR::oracle_create_table(table_name = "TMP_ADDRESSBASE_PLUS")

# Pull the temporary remote table
addressbase_plus_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM TMP_ADDRESSBASE_PLUS")
)

addressbase_plus_db <- addressbase_plus_db %>%
  dplyr::filter(UPRN == 4355)

# Where DPA and GEO have different single line addresses for the same UPRN, get
# a superset of all tokens
combined_addresses_db <- combined_addresses_db %>%
  dplyr::inner_join(
    y = addressbase_plus_db %>% 
      dplyr::select(UPRN, TOKEN, TOKEN_TYPE)
  ) %>%
  dplyr::distinct()

# Indicate where each token has originated
combined_addresses_db <- combined_addresses_db%>%
  dplyr::left_join(
    y = addressbase_plus_db %>%
      dplyr::filter(ADDRESS_TYPE == "DPA") %>%
      dplyr::mutate(DPA_TOKEN = 1) %>%
      dplyr::select(-ADDRESS_TYPE, -SINGLE_LINE_ADDRESS, -TOKEN_NUMBER)
  ) %>%
  dplyr::left_join(
    y = addressbase_plus_db %>%
      dplyr::filter(ADDRESS_TYPE == "GEO") %>%
      dplyr::mutate(GEO_TOKEN = 1) %>%
      dplyr::select(-ADDRESS_TYPE, -SINGLE_LINE_ADDRESS, -TOKEN_NUMBER)
  )

# Filter combined addresses where the one DPA / GEO address is a subset of the 
# other
combined_addresses_db <- combined_addresses_db %>%
  dplyr::inner_join(
    y = combined_addresses_db %>%
      dplyr::group_by(UPRN) %>%
      dplyr::summarise(
        MISSING_DPA_TOKENS = sum(ifelse(is.na(DPA_TOKEN), 1, 0)),
        MISSING_GEO_TOKENS = sum(ifelse(is.na(GEO_TOKEN), 1, 0))
      ) %>%
      # If one address is missing 0 tokens then we don't need a combined address
      dplyr::filter(MISSING_DPA_TOKENS > 0 & MISSING_GEO_TOKENS > 0) %>%
      dplyr::select(UPRN)
  ) %>%
  dplyr::select(-DPA_TOKEN, -GEO_TOKEN)

# Add the combined token rows to our data
addressbase_plus_db <- addressbase_plus_db %>%
  dplyr::union_all(
    y = combined_addresses_db %>%
      dplyr::mutate(ADDRESS_TYPE = "COMBINED")
  ) %>%
  dplyr::arrange(UPRN, ADDRESS_TYPE, TOKEN_NUMBER)

# Write the table back to the DB
addressbase_plus_db %>%
  nhsbsaR::oracle_create_table(table_name = "ADDRESSBASE_PLUS_CARE_HOME")

# Drop the temporary tables
DBI::dbRemoveTable(conn = con, name = "TMP_COMBINED_ADDRESSES")
DBI::dbRemoveTable(conn = con, name = "TMP_ADDRESSBASE_PLUS")

# Disconnect from database
DBI::dbDisconnect(con)
