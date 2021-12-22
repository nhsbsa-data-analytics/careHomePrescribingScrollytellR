# Libraries
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Results item table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Create a lazy table from the year month table
year_month_db <- con %>%
  tbl(from = in_schema("DALL_REF", "YEAR_MONTH_DIM"))

# Process fact table 
fact_db <- fact_db %>% 
  filter(CH_FLAG == 1)

# Process Year month table prior to join
year_month_db <- year_month_db %>%
  filter(FINANCIAL_YEAR == "2020/2021") %>%
  select(YEAR_MONTH)

# Function to process data
calc_rolling_pats = function(start_month, end_month){

  select_years <- year_month_db %>% 
    filter(YEAR_MONTH >= start_month & YEAR_MONTH <= end_month) %>% 
    mutate(
      FINAL_MONTH = end_month,
      NUM_MONTH = n(),
      NUM_MONTH = paste0(
        "Rolling ", NUM_MONTH, " Month Rolling Distinct Patient Count"
        )
      )
  
  fact_db %>% 
    select(YEAR_MONTH, NHS_NO) %>% 
    inner_join(select_years) %>% 
    group_by(FINAL_MONTH, NUM_MONTH) %>% 
    summarise(PATS = n_distinct(NHS_NO)) %>% 
    ungroup()
}

# Get 1 month static data
one_month_data <- fact_db %>% 
  select(YEAR_MONTH, NHS_NO) %>% 
  inner_join(year_month_db) %>% 
  group_by(YEAR_MONTH) %>% 
  summarise(PATS = n_distinct(NHS_NO)) %>% 
  ungroup() %>% 
  mutate(NUM_MONTH = "Single Month Distinct Patient Count") %>% 
  select(FINAL_MONTH = YEAR_MONTH, NUM_MONTH, PATS)

# Get 3 month rolling data
three_month_data <- calc_rolling_pats(202004, 202006) %>% 
  union_all(calc_rolling_pats(202005, 202007)) %>% 
  union_all(calc_rolling_pats(202006, 202008)) %>% 
  union_all(calc_rolling_pats(202007, 202009)) %>% 
  union_all(calc_rolling_pats(202008, 202010)) %>% 
  union_all(calc_rolling_pats(202009, 202011)) %>% 
  union_all(calc_rolling_pats(202010, 202012)) %>% 
  union_all(calc_rolling_pats(202011, 202101)) %>% 
  union_all(calc_rolling_pats(202012, 202102)) %>% 
  union_all(calc_rolling_pats(202101, 202103))

# Get  month rolling data
six_month_data <- calc_rolling_pats(202004, 202009) %>% 
  union_all(calc_rolling_pats(202005, 202010)) %>% 
  union_all(calc_rolling_pats(202006, 202011)) %>% 
  union_all(calc_rolling_pats(202007, 202012)) %>% 
  union_all(calc_rolling_pats(202008, 202101)) %>% 
  union_all(calc_rolling_pats(202009, 202102)) %>% 
  union_all(calc_rolling_pats(202010, 202103))

# Union all data
rolling_pat_count_df <- one_month_data %>% 
  union_all(three_month_data) %>% 
  union_all(six_month_data) %>% 
  collect() %>% 
  mutate(
    YEAR = substr(FINAL_MONTH, 1, 4),
    MONTH = month.abb[as.integer(substr(FINAL_MONTH, 5, 6))],
    YEAR_MONTH = paste0(MONTH, " - ", YEAR)
  ) %>% 
  select(-c(YEAR, MONTH))

# Add to data
usethis::use_data(
  rolling_pat_count_df,
  overwrite = TRUE
)

# Disconnect from database
DBI::dbDisconnect(con)