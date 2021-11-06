library(magrittr)

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the care home FACT table
addressbase_plus_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM DALL_REF.ADDRESSBASE_PLUS")
)

# Filter AddressBase Plus to English properties in at the end of 2021 FY and 
# create a care home flag
addressbase_plus_db <- addressbase_plus_db %>%
  dplyr::filter(
    COUNTRY == "E",
    substr(CLASS, 1, 1) != "L",
    substr(CLASS, 1, 1) != "Z",
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
addressbase_plus_db <-addressbase_plus_db %>%
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
different_addresses_db <- addressbase_plus_db %>%
  dplyr::group_by(UPRN) %>%
  dplyr::summarise(NO_ADDRESS_TYPE = dplyr::n_distinct(ADDRESS_TYPE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(NO_ADDRESS_TYPE > 1) %>%
  dplyr::select(UPRN) %>%
  dplyr::inner_join(addressbase_plus_db)

# Create a combined single line address for these of the form "{DPA} / {GEO}"
different_addresses_db <- different_addresses_db %>%
  tidyr::pivot_wider(
    id_cols = UPRN,
    names_from = ADDRESS_TYPE,
    values_from = SINGLE_LINE_ADDRESS
  ) %>%
  dplyr::mutate(SINGLE_LINE_ADDRESS = paste0(DPA, " / ", GEO)) %>%
  dplyr::select(UPRN, SINGLE_LINE_ADDRESS)

# Tokenise the single line addresses and add token type ("D" = digit, 
# "C" = character)
addressbase_plus_db <- addressbase_plus_db %>%
  nhsbsaR::oracle_unnest_tokens(col = "SINGLE_LINE_ADDRESS", drop = FALSE) %>%
  dplyr::mutate(TOKEN_TYPE = ifelse(REGEXP_LIKE(TOKEN, "[0-9]"), "D", "C"))

# Where DPA and GEO have different single line addresses for the same UPRN, get
# a superset of all tokens
different_addresses_combined_tokens_db <- addressbase_plus_db %>%
  dplyr::inner_join(y = different_addresses_db %>% dplyr::select(UPRN)) %>%
  dplyr::distinct(UPRN, CH_FLAG, POSTCODE, TOKEN, TOKEN_TYPE)

# Add the combined token rows to our data
addressbase_plus_db <- addressbase_plus_db %>%
  dplyr::union_all(
    y = different_addresses_combined_tokens_db %>%
      dplyr::inner_join(different_addresses_db) %>%
      dplyr::mutate(ADDRESS_TYPE = "COMBINED")
  ) %>%
  dplyr::arrange(UPRN, ADDRESS_TYPE, TOKEN_NUMBER)

# Write the table back to the DB
addressbase_plus_db %>%
  nhsbsaR::oracle_create_table(
    schema_name = Sys.getenv("DB_DALP_USERNAME"),
    table_name = "ADDRESSBASE_PLUS_CARE_HOME"
  )
