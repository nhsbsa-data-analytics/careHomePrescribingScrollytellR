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

# Combine the chapter / section / paragraph and description and filter to 2020/2021

drug_db <- drug_db %>%
  inner_join(year_month_db) %>%
  mutate(
    BNF_CHAPTER = paste0("(", BNF_CHAPTER, ") ", CHAPTER_DESCR),
    BNF_SECTION = paste0("(", BNF_SECTION, ") ", SECTION_DESCR),
    BNF_PARAGRAPH = PARAGRAPH_DESCR) %>%
  select(YEAR_MONTH, RECORD_ID, BNF_CHAPTER, BNF_SECTION, BNF_PARAGRAPH)

# Create a lazy table from the care home FACT table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Join the drug information to the FACT table and limit to care home patients

fact_db <- fact_db %>%
  # filter(CH_FLAG == 1) %>%
  inner_join(
    y = drug_db,
    by = c("YEAR_MONTH", "CALC_PREC_DRUG_RECORD_ID" = "RECORD_ID")
  )

# Get the total items and cost per BNF chapter, section (level 2)
# care home filter in here as fact_db will be needed for top 20 drugs
items_and_cost_per_bnf_chapter_and_section_df <- fact_db %>%
  filter(CH_FLAG == 1) %>%
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

# This step is for another data frame for dumbbell chart

ch_items_and_cost_per_bnf_para_df <- fact_db %>%
  group_by(CH_FLAG, BNF_PARAGRAPH) %>%
  summarise(
    Items = sum(ITEM_COUNT),
    Cost = sum(ITEM_PAY_DR_NIC * 0.01)
  ) %>%
  ungroup() %>% 
  collect() %>% 
  group_by(CH_FLAG) %>% 
  mutate(Items_p = Items / sum(Items),
         Cost_p = Cost / sum(Cost)) %>% 
  ungroup()

# create Items related table
# top 20 in ch
ch_items_top_20_df <- ch_items_and_cost_per_bnf_para_df %>% 
  filter(CH_FLAG == 1) %>% 
  slice_max(Items_p, n = 20) %>% 
  select(CH_FLAG, BNF_PARAGRAPH, Items_p) %>% 
  mutate(CH_P = Items_p) %>% 
  select(-c(Items_p, CH_FLAG))

# pull top 20 to filter from non care home
top20 <- ch_items_top_20_df %>% 
  pull(BNF_PARAGRAPH)

# get equivalent percent in none-ch
none_ch_item_20_df <- ch_items_and_cost_per_bnf_para_df %>% 
  filter(CH_FLAG == 0 ) %>% 
  filter(BNF_PARAGRAPH %in% top20) %>% 
  select(CH_FLAG, BNF_PARAGRAPH, Items_p) %>% 
  mutate(NONE_CH_P = Items_p) %>% 
  select(-c(Items_p,CH_FLAG))

# item df for dumbbell chart
top20_item_df <- ch_items_top_20_df %>% 
  inner_join(y = none_ch_item_20_df,
             by = "BNF_PARAGRAPH") %>% 
  mutate(METRIC = "Items")


# repeat again for NIC 

ch_nic_top_20_df <- ch_items_and_cost_per_bnf_para_df %>% 
  filter(CH_FLAG == 1) %>% 
  slice_max(Cost_p, n = 20) %>% 
  select(CH_FLAG, BNF_PARAGRAPH, Cost_p) %>% 
  mutate(CH_P = Cost_p) %>% 
  select(-c(Cost_p, CH_FLAG))

# pull top 20 to filter from non care home
top20_nic <- ch_nic_top_20_df %>% 
  pull(BNF_PARAGRAPH)

# get equivalent percent in none-ch
none_ch_cost_20_df <- ch_items_and_cost_per_bnf_para_df %>% 
  filter(CH_FLAG == 0 ) %>% 
  filter(BNF_PARAGRAPH %in% top20_nic) %>% 
  select(CH_FLAG, BNF_PARAGRAPH, Cost_p) %>% 
  mutate(NONE_CH_P = Cost_p) %>% 
  select(-c(Cost_p,CH_FLAG))

top20_cost_df <- ch_nic_top_20_df %>% 
  inner_join(y = none_ch_cost_20_df,
             by = "BNF_PARAGRAPH") %>% 
  mutate(METRIC = "Costs")

# stack them 
top20_df <- bind_rows(top20_cost_df, top20_item_df)



# Add to data-raw/
usethis::use_data(
  items_and_cost_per_bnf_chapter_and_section_df, 
  overwrite = TRUE
)

usethis::use_data(
  top20_df,
  overwrite = TRUE
)

# Disconnect from database
DBI::dbDisconnect(con)
