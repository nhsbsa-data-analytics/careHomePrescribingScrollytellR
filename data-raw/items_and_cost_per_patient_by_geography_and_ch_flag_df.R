library(dplyr)
library(dbplyr)
devtools::load_all()

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
  mutate(OVERALL_CODE = NA, OVERALL_NAME = "Overall") # dummy col 
  
# Loop over geography cols and aggregate
for (geography in c("OVERALL", "PCD_REGION", "PCD_STP", "PCD_LAD")) {
  
  # Monthly cost per patient by care home flag
  tmp_db <- fact_db %>%
    group_by(
      YEAR_MONTH = as.character(YEAR_MONTH), 
      GEOGRAPHY = switch(
        geography,
        "OVERALL" = "Overall",
        "PCD_REGION" = "Region",
        "PCD_STP" = "STP",
        "PCD_LAD" = "Local Authority"
      ), 
      SUB_GEOGRAPHY_CODE = !!dplyr::sym(paste0(geography, "_CODE")),
      SUB_GEOGRAPHY_NAME = !!dplyr::sym(paste0(geography, "_NAME")),
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
        group_by(
          YEAR_MONTH = "Overall", 
          GEOGRAPHY, 
          SUB_GEOGRAPHY_CODE,
          SUB_GEOGRAPHY_NAME, 
          CH_FLAG
        ) %>%
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

# Collect and format for highcharter
items_and_cost_per_patient_by_geography_and_ch_flag_df <- 
  items_and_cost_per_patient_by_geography_and_ch_flag_db %>%
  collect() %>%
  careHomePrescribingScrollytellR::format_data_raw(CH_FLAG)

# Add to data-raw/
usethis::use_data(
  items_and_cost_per_patient_by_geography_and_ch_flag_df, 
  overwrite = TRUE
)

# Disconnect from database
DBI::dbDisconnect(con)
