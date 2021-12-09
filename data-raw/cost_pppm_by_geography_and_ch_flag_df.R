# Load libraries
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the geography lookup table (Region, STP and LA)
postcode_db <- con %>%
  tbl(from = in_schema("ADAIV", "INT615_PCD_REF"))

# Subset the columns
postcode_db <- postcode_db %>%
  select(
    PCD_NO_SPACES = POSTCODE, # to join to FACT table
    PCD_REGION_NAME, 
    PCD_STP_NAME, 
    PCD_LAD_NAME
  )

# Create a lazy table from the care home FACT table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Tidy care home flag and join the postcode info
fact_db <- fact_db %>%
  mutate(CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home")) %>%
  left_join(
    y = postcode_db, 
    copy = TRUE
  )  %>%
  mutate(OVERALL = "Overall") %>% # dummy col so aggregation is easier
  
# Loop over geography cols and aggregate
for (
  geography in c("OVERALL", "PCD_REGION_NAME", "PCD_STP_NAME", "PCD_LAD_NAME")
) {
  
  # Monthly cost per patient by care home flag
  tmp_df <- fact_db %>%
    group_by(
      YEAR_MONTH = as.character(YEAR_MONTH), 
      GEOGRAPHY = geography, 
      SUB_GEOGRAPHY = !!dplyr::sym(geography),
      CH_FLAG, 
    ) %>%
    summarise(
      TOTAL_COST = sum(ITEM_PAY_DR_NIC * 0.01),
      TOTAL_PATIENTS = n_distinct(NHS_NO),
      COST_PER_PATIENT = sum(ITEM_PAY_DR_NIC * 0.01) / n_distinct(NHS_NO)
    ) %>%
    ungroup()
  
  # Add overall mean (average monthly cost per patient is the metric)
  tmp_df <- tmp_df %>%
    union_all(
      y = tmp_df %>%
        group_by(YEAR_MONTH = "Overall", GEOGRAPHY, SUB_GEOGRAPHY, CH_FLAG) %>%
        summarise(COST_PER_PATIENT = mean(COST_PER_PATIENT)) %>%
        ungroup()
    )
  
  # Either create the table or append to it
  if (geography == "OVERALL") {
    
    # On the first iteration initialise the table
    cost_pppm_by_geography_and_ch_flag_db <- tmp_df
    
  } else {
    
    # Union results to initialised table
    cost_pppm_by_geography_and_ch_flag_db <- union_all(
      x = cost_pppm_by_geography_and_ch_flag_db,
      y = tmp_df
    )
    
  }
  
}

# Give the GEOGRAPHY column nice names
cost_pppm_by_geography_and_ch_flag_db <- 
  cost_pppm_by_geography_and_ch_flag_db %>%
  mutate(
    GEOGRAPHY = case_when(
      GEOGRAPHY == "OVERALL" ~ "Overall",
      GEOGRAPHY == "PCD_REGION_NAME" ~ "Region",
      GEOGRAPHY == "PCD_STP_NAME" ~ "STP",
      GEOGRAPHY == "PCD_LAD_NAME" ~ "Local Authority"
    )
  )

# Sort as is (not geography as we do that later) and collect
cost_pppm_by_geography_and_ch_flag_df <- 
  cost_pppm_by_geography_and_ch_flag_db %>%
  arrange(YEAR_MONTH, SUB_GEOGRAPHY, CH_FLAG) %>%
  collect() 
  
# Format for highcharter
cost_pppm_by_geography_and_ch_flag_df <- 
  cost_pppm_by_geography_and_ch_flag_df %>%
  # Tweak the factors
  mutate(
    # Move overall to first category
    across(
      .cols = c(YEAR_MONTH, SUB_GEOGRAPHY),
      .fns = ~ forcats::fct_relevel(.x, "Overall")
    ),
    # Factor is a heirachy
    GEOGRAPHY = forcats::fct_relevel(GEOGRAPHY, "Overall", "Region", "STP")
  ) %>%
  # Sort final dataframe by new factors
  arrange(YEAR_MONTH, GEOGRAPHY, SUB_GEOGRAPHY, CH_FLAG)

# Add to data-raw/
usethis::use_data(cost_pppm_by_geography_and_ch_flag_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)
