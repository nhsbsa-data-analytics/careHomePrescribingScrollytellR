# Load libraries
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the geography mapping table
geography_db <- con %>%
  tbl(from = in_schema("DALL_REF", "ONS_GEOGRAPHY_MAPPING"))

# Create a lazy table from the postcode data table
postcode_db <- con %>%
  tbl(from = in_schema("DIM", sql("ONS_POSTCODE_DATA_DIM@dwcpb")))

# Create a lazy table for IMD data
imd_db <- con %>%
  tbl(from = in_schema("DALL_REF", "ONS_INDEX_OF_MULTIPLE_DEPRIVATION"))

# Check if the table exists
exists <- DBI::dbExistsTable(conn = con, name = "INT615_POSTCODE_LOOKUP")

# Drop any existing table beforehand
if (exists) {
  DBI::dbRemoveTable(conn = con, name = "INT615_POSTCODE_LOOKUP")
}

# Get the latest postcode data for each postcode
postcode_db <- postcode_db %>%
  group_by(POSTCODE) %>%
  window_order(desc(YEAR_MONTH)) %>%
  mutate(RANK = rank()) %>%
  filter(RANK == 1) %>%
  select(POSTCODE, LSOA_CODE = CENSUS_LOWER, YEAR_MONTH) %>%
  addressMatchR::tidy_postcode(POSTCODE)

# Join to the postcode lookup to get region, STP and LA
postcode_db <- postcode_db %>%
  # STP
  inner_join(
    y = geography_db %>%
      filter(RELATIONSHIP == "LSOA_STP2021") %>%
      select(
        LSOA_CODE = CHILD_ONS_CODE,
        PCD_STP_CODE = PARENT_ONS_CODE,
        PCD_STP_NAME = PARENT_NAME
      )
  ) %>%
  # LA
  inner_join(
    y = geography_db %>%
      filter(RELATIONSHIP == "LSOA_LAD2021") %>%
      select(
        LSOA_CODE = CHILD_ONS_CODE,
        PCD_LAD_CODE = PARENT_ONS_CODE,
        PCD_LAD_NAME = PARENT_NAME
      )
  ) %>%
  # Region
  inner_join(
    y = geography_db %>%
      filter(RELATIONSHIP == "LAD2021_REG2021") %>%
      select(
        PCD_LAD_CODE = CHILD_ONS_CODE,
        PCD_REGION_CODE = PARENT_ONS_CODE,
        PCD_REGION_NAME = PARENT_NAME
      )
  ) %>%
  # Index of Multiple Deprivation
  left_join(
    y = imd_db %>%
      filter(IMD_YEAR == 2019) %>%
      mutate(IMD_QUINTILE = round(INDEX_OF_MULT_DEPRIV_DECILE / 2, 0)) %>%
      select(LSOA_CODE, IMD_QUINTILE)
  )

# Reorder the columns
postcode_db <- postcode_db %>%
  select(
    POSTCODE,
    PCD_REGION_CODE,
    PCD_REGION_NAME,
    PCD_STP_CODE,
    PCD_STP_NAME,
    PCD_LAD_CODE,
    PCD_LAD_NAME,
    IMD_QUINTILE
  )

# Write the table back to the DB
postcode_db %>%
  nhsbsaR::oracle_create_table(table_name = "INT615_POSTCODE_LOOKUP")

# Disconnect from database
DBI::dbDisconnect(con)
