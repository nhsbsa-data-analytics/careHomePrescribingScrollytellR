library(dplyr)
library(dbplyr)
devtools::load_all()

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the geography lookup table (Region, STP and LA)
postcode_db <- con %>%
  tbl(from = "INT615_POSTCODE_LOOKUP")

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

# Filter to BNF chapters 1 4 and 6 - 10 in 2020/2021 and subset columns
drug_db <- drug_db %>%
  filter(BNF_CHAPTER %in% c(01, 02, 03, 04, 06, 07, 08, 09, 10)) %>%
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
    y = postcode_db %>% rename(PCD_NO_SPACES = POSTCODE), 
    copy = TRUE
  ) %>%
  mutate(OVERALL_CODE = NA, OVERALL_NAME = "Overall") # dummy col

# Loop over geography cols and aggregate
for (geography in c("OVERALL", "PCD_REGION", "PCD_STP", "PCD_LAD")) {
  
  # Get the number of unique medicines per patient
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
      NHS_NO
    ) %>%
    summarise(UNIQUE_MEDICINES = n_distinct(CHEMICAL_SUBSTANCE_BNF_DESCR)) %>%
    ungroup()
  
  # Summarise across patients
  tmp_db <- tmp_db %>%
    group_by(
      YEAR_MONTH,
      GEOGRAPHY,
      SUB_GEOGRAPHY_CODE,
      SUB_GEOGRAPHY_NAME,
      CH_FLAG
    ) %>%
    summarise(
      # Unique medicines per patient
      UNIQUE_MEDICINES_PER_PATIENT = mean(UNIQUE_MEDICINES),
      # % of patients on ten or more unique medicines 
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
          UNIQUE_MEDICINES_PER_PATIENT = mean(UNIQUE_MEDICINES_PER_PATIENT),
          PCT_PATIENTS_TEN_OR_MORE = mean(PCT_PATIENTS_TEN_OR_MORE)
        ) %>%
        ungroup()
    )
  
  # Either create the table or append to them
  if (geography == "OVERALL") {
    
    # On the first iteration initialise the table
    unique_medicines_per_patient_by_geography_db <- tmp_db
    
  } else {
    
    # Union results to initialised table
    unique_medicines_per_patient_by_geography_db <- union_all(
      x = unique_medicines_per_patient_by_geography_db,
      y = tmp_db
    )
    
  }
  
}

# Collect and format for highcharter
unique_medicines_per_patient_by_geography_df <- 
  unique_medicines_per_patient_by_geography_db %>%
  collect() %>%
  careHomePrescribingScrollytellR::format_data_raw(CH_FLAG)


# Add to data-raw/
usethis::use_data(
  unique_medicines_per_patient_by_geography_df, 
  overwrite = TRUE
)

# Disconnect from database
DBI::dbDisconnect(con)
