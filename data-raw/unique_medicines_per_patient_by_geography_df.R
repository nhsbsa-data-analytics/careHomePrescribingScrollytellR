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

# Create a lazy table from the year month table
year_month_db <- con %>%
  tbl(from = in_schema("DALL_REF", "YEAR_MONTH_DIM"))

# Filter to 2020/2021
year_month_db <- year_month_db %>%
  filter(FINANCIAL_YEAR == "2020/2021") %>%
  select(YEAR_MONTH)

# Create a lazy table from the drug DIM table
drug_db <- con %>%
  tbl(from = in_schema("SB_DIM", "CDR_DRUG_BNF_DIM"))

# Filter to BNF chapters 1 - 10 in 2020/2021 and subset columns
drug_db <- drug_db %>%
  filter(BNF_CHAPTER %in% c(01, 02, 03, 04, 05, 06, 07, 08, 09, 10)) %>%
  inner_join(year_month_db) %>%
  select(YEAR_MONTH, RECORD_ID, CHEMICAL_SUBSTANCE_BNF_DESCR)

# Create a lazy table from the care home FACT table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))
  
# Tidy care home flag, join drug information and postcode info to the FACT table
fact_db <- fact_db %>%
  mutate(CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home")) %>%
  inner_join(
    y = drug_db,
    by = c("YEAR_MONTH", "CALC_PREC_DRUG_RECORD_ID" = "RECORD_ID")
  ) %>%
  left_join(
    y = postcode_db, 
    copy = TRUE
  ) %>%
  mutate(OVERALL = "Overall") # dummy col so aggregation is easier

# Loop over geography cols and aggregate
for (
  geography in c("OVERALL", "PCD_REGION_NAME", "PCD_STP_NAME", "PCD_LAD_NAME")
) {
  
  # Unique medicines
  tmp_unique_medicines_db <- fact_db %>%
    group_by(
      YEAR_MONTH = as.character(YEAR_MONTH), 
      GEOGRAPHY = geography, 
      SUB_GEOGRAPHY = !!dplyr::sym(geography),
      CH_FLAG,
      NHS_NO
    ) %>%
    summarise(UNIQUE_MEDICINES = n_distinct(CHEMICAL_SUBSTANCE_BNF_DESCR)) %>%
    ungroup()
  
  # Average unique medicines per patient
  
  # Number of unique medicines per patient per month by care home flag
  tmp_unique_medicines_per_patient_by_geography_db <- 
    tmp_unique_medicines_db %>%
    group_by(
      YEAR_MONTH = as.character(YEAR_MONTH), 
      GEOGRAPHY, 
      SUB_GEOGRAPHY,
      CH_FLAG
    ) %>%
    summarise(UNIQUE_MEDICINES_PER_PATIENT = mean(UNIQUE_MEDICINES)) %>%
    ungroup()
  
  # Add overall mean (average monthly per patient is the metric)
  tmp_unique_medicines_per_patient_by_geography_db <- 
    tmp_unique_medicines_per_patient_by_geography_db %>%
    union_all(
      y = tmp_unique_medicines_per_patient_by_geography_db %>%
        group_by(YEAR_MONTH = "Overall", GEOGRAPHY, SUB_GEOGRAPHY, CH_FLAG) %>%
        summarise(
          UNIQUE_MEDICINES_PER_PATIENT = mean(UNIQUE_MEDICINES_PER_PATIENT)
        ) %>%
        ungroup()
    )
  
  # Ten or more unique medicines per patient
  
  # Ten or more unique medicines per patient per month by care home flag
  tmp_ten_or_more_unique_medicines_per_patient_by_geography_db <- 
    tmp_unique_medicines_db %>%
    group_by(
      YEAR_MONTH = as.character(YEAR_MONTH), 
      GEOGRAPHY, 
      SUB_GEOGRAPHY,
      CH_FLAG
    ) %>%
    summarise(
      PATIENTS_TEN_OR_MORE = n_distinct(
        ifelse(UNIQUE_MEDICINES >= 10, NHS_NO, NA)
      ),
      TOTAL_PATIENTS = n_distinct(NHS_NO)
    ) %>%
    ungroup() %>%
    mutate(
      PCT_PATIENTS_TEN_OR_MORE = PATIENTS_TEN_OR_MORE / TOTAL_PATIENTS * 100
    ) %>%
    select(-c(PATIENTS_TEN_OR_MORE, TOTAL_PATIENTS))
  
  # Add overall mean (average monthly per patient is the metric)
  tmp_ten_or_more_unique_medicines_per_patient_by_geography_db <- 
    tmp_ten_or_more_unique_medicines_per_patient_by_geography_db %>%
    union_all(
      y = tmp_ten_or_more_unique_medicines_per_patient_by_geography_db %>%
        group_by(YEAR_MONTH = "Overall", GEOGRAPHY, SUB_GEOGRAPHY, CH_FLAG) %>%
        summarise(
          PCT_PATIENTS_TEN_OR_MORE = mean(PCT_PATIENTS_TEN_OR_MORE)
        ) %>%
        ungroup()
    )
  
  # Either create the tables or append to them
  if (geography == "OVERALL") {
    
    # On the first iteration initialise the tables
    unique_medicines_per_patient_by_geography_db <- 
      tmp_unique_medicines_per_patient_by_geography_db
    ten_or_more_unique_medicines_per_patient_by_geography_db <- 
      tmp_ten_or_more_unique_medicines_per_patient_by_geography_db
    
  } else {
    
    # Union results to initialised tables
    unique_medicines_per_patient_by_geography_db <- union_all(
      x = unique_medicines_per_patient_by_geography_db,
      y = tmp_unique_medicines_per_patient_by_geography_db
    )
    ten_or_more_unique_medicines_per_patient_by_geography_db <- union_all(
      x = ten_or_more_unique_medicines_per_patient_by_geography_db,
      y = tmp_ten_or_more_unique_medicines_per_patient_by_geography_db
    )
    
  }
  
}

# Give the GEOGRAPHY column nice names

unique_medicines_per_patient_by_geography_db <- 
  unique_medicines_per_patient_by_geography_db %>%
  mutate(
    GEOGRAPHY = case_when(
      GEOGRAPHY == "OVERALL" ~ "Overall",
      GEOGRAPHY == "PCD_REGION_NAME" ~ "Region",
      GEOGRAPHY == "PCD_STP_NAME" ~ "STP",
      GEOGRAPHY == "PCD_LAD_NAME" ~ "Local Authority"
    )
  )

ten_or_more_unique_medicines_per_patient_by_geography_db <- 
  ten_or_more_unique_medicines_per_patient_by_geography_db %>%
  mutate(
    GEOGRAPHY = case_when(
      GEOGRAPHY == "OVERALL" ~ "Overall",
      GEOGRAPHY == "PCD_REGION_NAME" ~ "Region",
      GEOGRAPHY == "PCD_STP_NAME" ~ "STP",
      GEOGRAPHY == "PCD_LAD_NAME" ~ "Local Authority"
    )
  )

# Sort as is (not geography as we do that later) and collect

unique_medicines_per_patient_by_geography_df <- 
  unique_medicines_per_patient_by_geography_db %>%
  arrange(YEAR_MONTH, SUB_GEOGRAPHY, CH_FLAG) %>%
  collect() 

ten_or_more_unique_medicines_per_patient_by_geography_df <- 
  ten_or_more_unique_medicines_per_patient_by_geography_db %>%
  arrange(YEAR_MONTH, SUB_GEOGRAPHY, CH_FLAG) %>%
  collect() 

# Format for highcharter

unique_medicines_per_patient_by_geography_df <- 
  unique_medicines_per_patient_by_geography_df %>%
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

ten_or_more_unique_medicines_per_patient_by_geography_df <- 
  ten_or_more_unique_medicines_per_patient_by_geography_df %>%
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
  unique_medicines_per_patient_by_geography_df, 
  overwrite = TRUE
)
usethis::use_data(
  ten_or_more_unique_medicines_per_patient_by_geography_df, 
  overwrite = TRUE
)

# Disconnect from database
DBI::dbDisconnect(con)
