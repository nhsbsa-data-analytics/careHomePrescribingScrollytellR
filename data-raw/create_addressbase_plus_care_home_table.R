library(magrittr)

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the care home FACT table
addressbase_plus_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM DALL_REF.ADDRESSBASE_PLUS")
)

# Filter AddressBase Plus for English properties in 2020 / 2021
addressbase_plus_db <- addressbase_plus_db %>%
  dplyr::filter(

    # England only
    COUNTRY == "E",

    # Undesired classes
    substr(CLASS, 1, 1) != "L",
    substr(CLASS, 1, 1) != "Z",

    # Required periods
    RELEASE_DATE > to_date("2020-04-23", "YYYY:MM:DD"),
    RELEASE_DATE <= to_date("2021-03-15", "YYYY:MM:DD")
  )

# Get postcodes where there is a care home present
care_home_postcodes_db <- addressbase_plus_db %>%
  dplyr::filter(CLASS == "RI01") %>%
  dplyr::select(POSTCODE, POSTCODE_LOCATOR) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    values_to = "POSTCODE",
    values_drop_na = TRUE
  ) %>%
  dplyr::distinct(POSTCODE)

# Filter AddressBase Plus to postcodes where there is a care home present
addressbase_plus_db <- addressbase_plus_db %>%
  dplyr::left_join(
    y = care_home_postcodes_db %>% dplyr::mutate(CH_IN_POSTCODE = 1),
    by = c("POSTCODE" = "POSTCODE")
  ) %>%
  dplyr::filter(CH_IN_POSTCODE == 1)

# Create a long table of single line addresses
addressbase_plus_db <- addressbase_plus_db %>%
  addressMatchR::calc_addressbase_plus_dpa_single_line_address() %>%
  addressMatchR::calc_addressbase_plus_geo_single_line_address() %>%
  dplyr::select(
    UPRN,
    CLASS,
    DPA_POSTCODE = POSTCODE,
    DPA_SINGLE_LINE_ADDRESS,
    GEO_POSTCODE = POSTCODE,
    GEO_SINGLE_LINE_ADDRESS
  ) %>%
  tidyr::pivot_longer(
    cols = -c(UPRN, CLASS),
    names_to = c("ADDRESS_TYPE", ".value"),
    names_sep = "_" # Should use names_pattern but can't get it to work
  ) %>%
  # Hack the names_sep result back to what it should be (ignore warning as 
  # splits {DPA,GEO}_SINGLE_LINE_ADDRESS into {DPA,GEO} / SINGLE / LINE / 
  # ADDRESS and drops LINE / ADDRESS as no data exists)
  dplyr::rename(SINGLE_LINE_ADDRESS = SINGLE)

# Format the postcodes and single line addresses
addressbase_plus_db <- addressbase_plus_db %>%
  # Remove whitespace from postcode
  dplyr::mutate(POSTCODE = REGEXP_REPLACE(POSTCODE, " ", "")) %>%
  # Tidy single line address for tokenisation
  addressMatchR::tidy_single_line_address(col = SINGLE_LINE_ADDRESS)

# Drop any duplicates and missing single line addresses
addressbase_plus_db <- addressbase_plus_db %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(SINGLE_LINE_ADDRESS))

# Tokenise the single line addresses
tmp_df <- addressbase_plus_db %>%
  # Test on 5 addresses
  head() %>%
  nhsbsaR::oracle_unnest_tokens(col = "SINGLE_LINE_ADDRESS", drop = FALSE) %>%
  dplyr::collect()
