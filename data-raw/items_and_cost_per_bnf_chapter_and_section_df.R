# Load libraries
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the year month table
year_month_db <- con %>%
  tbl(from = in_schema("DALL_REF", "YEAR_MONTH_DIM"))

# Filter to 2020/2021
year_month_db <- year_month_db %>%
  filter(FINANCIAL_YEAR == "2020/2021") %>%
  select(YEAR_MONTH)

# Create a lazy table from the care home FACT table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Create a lazy table from the drug DIM table
drug_db <- con %>%
  tbl(from = in_schema("SB_DIM", "CDR_DRUG_BNF_DIM"))

# Combine the chapter / section and description and filter to 2020/2021
drug_db <- drug_db %>%
  inner_join(year_month_db) %>%
  mutate(
    BNF_CHAPTER = paste0("(", BNF_CHAPTER, ") ", CHAPTER_DESCR),
    BNF_SECTION = paste0("(", BNF_SECTION, ") ", SECTION_DESCR)
  ) %>%
  select(YEAR_MONTH, RECORD_ID, BNF_CHAPTER, BNF_SECTION)

# Create a lazy table from the care home FACT table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Join the drug information to the FACT table and limit to care home patients
# only
fact_db <- fact_db %>%
  filter(CH_FLAG == 1) %>%
  inner_join(
    y = drug_db,
    by = c("YEAR_MONTH", "CALC_PREC_DRUG_RECORD_ID" = "RECORD_ID")
  )

# Get the total items and cost per BNF chapter and section (level 2)
items_and_cost_per_bnf_chapter_and_section_df <- fact_db %>%
  group_by(BNF_CHAPTER, BNF_SECTION) %>%
  summarise(
    Items = sum(ITEM_COUNT),
    Cost = sum(ITEM_PAY_DR_NIC * 0.01)
  ) %>%
  ungroup() %>%
  tidyr::pivot_longer(
    cols = -c(BNF_CHAPTER, BNF_SECTION),
    names_to = "METRIC",
    values_to = "TOTAL_LEVEL_2"
  ) %>%
  collect()

# Group any BNF sections ranked 8th or less into a group called "Other"
items_and_cost_per_bnf_chapter_and_section_df <-
  items_and_cost_per_bnf_chapter_and_section_df %>%
  group_by(METRIC, BNF_CHAPTER) %>%
  mutate(
    BNF_SECTION = forcats::fct_lump(BNF_SECTION, n = 7, w = TOTAL_LEVEL_2)
  ) %>%
  ungroup() %>%
  group_by(METRIC, BNF_CHAPTER, BNF_SECTION) %>%
  summarise(TOTAL_LEVEL_2 = sum(TOTAL_LEVEL_2)) %>%
  ungroup() %>%
  # Add %s
  group_by(METRIC, BNF_CHAPTER) %>%
  mutate(PRP_LEVEL_2 = TOTAL_LEVEL_2 / sum(TOTAL_LEVEL_2)) %>%
  ungroup()


# Get the total items and cost per BNF chapter (level 1)
items_and_cost_per_bnf_chapter_df <-
  items_and_cost_per_bnf_chapter_and_section_df %>%
  group_by(METRIC, BNF_CHAPTER) %>%
  summarise(TOTAL_LEVEL_1 = sum(TOTAL_LEVEL_2)) %>%
  ungroup()

# Group any BNF chapters ranked 8th or less into a group called "Other"
items_and_cost_per_bnf_chapter_df <- items_and_cost_per_bnf_chapter_df %>%
  group_by(
    METRIC,
    BNF_CHAPTER = forcats::fct_lump(BNF_CHAPTER, n = 7, w = TOTAL_LEVEL_1)
  ) %>%
  summarise(TOTAL_LEVEL_1 = sum(TOTAL_LEVEL_1)) %>%
  ungroup() %>%
  # Add %s
  group_by(METRIC) %>%
  mutate(PRP_LEVEL_1 = TOTAL_LEVEL_1 / sum(TOTAL_LEVEL_1)) %>%
  ungroup()

# Join the two datasets together
items_and_cost_per_bnf_chapter_and_section_df <-
  left_join(
    x = items_and_cost_per_bnf_chapter_df,
    y = items_and_cost_per_bnf_chapter_and_section_df
  )

# Add to data-raw/
usethis::use_data(
  items_and_cost_per_bnf_chapter_and_section_df,
  overwrite = TRUE
)

# Disconnect from database
DBI::dbDisconnect(con)
