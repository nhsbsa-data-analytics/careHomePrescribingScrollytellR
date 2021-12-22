# Library
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Results item table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Drug info
drug_db <- con %>%
  tbl(from = in_schema("DIM", "CDR_EP_DRUG_BNF_DIM")) %>%
  select(YEAR_MONTH, RECORD_ID, CHAPTER_DESCR)

# Join and process tables then collect aggregated data
chapter_drug_comparison_df <- fact_db %>%
  select(
    ADDRESS_RECORD_ID,
    CALC_PREC_DRUG_RECORD_ID,
    CH_FLAG,
    ITEM_COUNT,
    YEAR_MONTH
    ) %>% 
  inner_join(
    y = drug_db,
    by = c("YEAR_MONTH", "CALC_PREC_DRUG_RECORD_ID" = "RECORD_ID")
  ) %>% 
  group_by(CHAPTER_DESCR, CH_FLAG) %>%
  summarise(ITEMS = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  group_by(CH_FLAG) %>% 
  mutate(PROP = ITEMS / sum(ITEMS)) %>% 
  ungroup() %>% 
  select(CHAPTER_DESCR, CH_FLAG, PROP) %>% 
  collect()

# Add to data
usethis::use_data(
  chapter_drug_comparison_df,
  overwrite = TRUE
)

# Disconnect from database
DBI::dbDisconnect(con)