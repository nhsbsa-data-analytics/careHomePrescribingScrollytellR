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

# Tidy care home flag and join the postcode info
fact_db <- fact_db %>%
  mutate(CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home")) %>%
  left_join(
    y = postcode_db %>% rename(PCD_NO_SPACES = POSTCODE), 
    copy = TRUE
  )  %>%
  mutate(OVERALL = "Overall") # dummy col so aggregation is easier
  
# Loop over geography cols and aggregate
for (
  geography in c("OVERALL", "PCD_REGION_NAME", "PCD_STP_NAME", "PCD_LAD_NAME")
) {
  
  # Monthly cost per patient by care home flag
  tmp_db <- fact_db %>%
    group_by(
      YEAR_MONTH = as.character(YEAR_MONTH), 
      GEOGRAPHY = geography, 
      SUB_GEOGRAPHY = !!dplyr::sym(geography),
      CH_FLAG, 
    ) %>%
    summarise(
      TOTAL_COST = sum(ITEM_PAY_DR_NIC * 0.01),
      TOTAL_ITEMS = sum(ITEM_COUNT),
      TOTAL_PATIENTS = n_distinct(NHS_NO),
      COST_PER_PATIENT = sum(ITEM_PAY_DR_NIC * 0.01) / n_distinct(NHS_NO),
      ITEMS_PER_PATIENT = sum(ITEM_COUNT) / n_distinct(NHS_NO)
    ) %>%
    ungroup()
  
  # Add overall mean (average monthly per patient is the metric)
  tmp_db <- tmp_db %>%
    union_all(
      y = tmp_db %>%
        group_by(YEAR_MONTH = "Overall", GEOGRAPHY, SUB_GEOGRAPHY, CH_FLAG) %>%
        summarise(
          COST_PER_PATIENT = mean(COST_PER_PATIENT),
          ITEMS_PER_PATIENT = mean(ITEMS_PER_PATIENT)
        ) %>%
        ungroup()
    )
  
  # Either create the table or append to it
  if (geography == "OVERALL") {
    
    # On the first iteration initialise the table
    items_and_cost_per_patient_by_geography_and_ch_flag_db <- tmp_db
    
  } else {
    
    # Union results to initialised table
    items_and_cost_per_patient_by_geography_and_ch_flag_db <- union_all(
      x = items_and_cost_per_patient_by_geography_and_ch_flag_db,
      y = tmp_db
    )
    
  }
  
}

# Give the GEOGRAPHY column nice names
items_and_cost_per_patient_by_geography_and_ch_flag_db <- 
  items_and_cost_per_patient_by_geography_and_ch_flag_db %>%
  mutate(
    GEOGRAPHY = case_when(
      GEOGRAPHY == "OVERALL" ~ "Overall",
      GEOGRAPHY == "PCD_REGION_NAME" ~ "Region",
      GEOGRAPHY == "PCD_STP_NAME" ~ "STP",
      GEOGRAPHY == "PCD_LAD_NAME" ~ "Local Authority"
    )
  )

# Sort as is (not geography as we do that later) and collect
items_and_cost_per_patient_by_geography_and_ch_flag_df <- 
  items_and_cost_per_patient_by_geography_and_ch_flag_db %>%
  arrange(YEAR_MONTH, SUB_GEOGRAPHY, CH_FLAG) %>%
  collect() 
  
# Format for highcharter
items_and_cost_per_patient_by_geography_and_ch_flag_df <- 
  items_and_cost_per_patient_by_geography_and_ch_flag_df %>%
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
usethis::use_data(
  items_and_cost_per_patient_by_geography_and_ch_flag_df, 
  overwrite = TRUE
)

# Disconnect from database
DBI::dbDisconnect(con)
