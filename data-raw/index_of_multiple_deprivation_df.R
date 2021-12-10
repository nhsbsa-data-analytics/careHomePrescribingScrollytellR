library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the geography lookup table (Region, STP and LA)
postcode_db <- con %>%
  tbl(from = "INT615_POSTCODE_LOOKUP")

# Create a lazy table from the care home FACT table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Join postcode_db and fact_db on postcode
fact_db <- fact_db %>%
  left_join(
    y = postcode_db,
    by = c("PCD_NO_SPACES" = "POSTCODE")
  )

# Calculate IMD quintile figures, filter to carehomes only
index_of_multiple_deprivation_df <- fact_db %>%
  group_by(IMD_QUINTILE) %>%
  filter(
    CH_FLAG == 1,
    !is.na(IMD_QUINTILE)
  ) %>%
  ungroup() %>%
  group_by(IMD_QUINTILE) %>%
  summarise(TOTAL_ITEMS = count(ITEM_COUNT)) %>%
  mutate(OVERALL_QUINTILE = sum(TOTAL_ITEMS),
         PROP = round(TOTAL_ITEMS / OVERALL_QUINTILE * 100, 0)) %>%
  arrange(IMD_QUINTILE) %>%
  collect()
  
# Add to data-raw/
usethis::use_data(index_of_multiple_deprivation_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)