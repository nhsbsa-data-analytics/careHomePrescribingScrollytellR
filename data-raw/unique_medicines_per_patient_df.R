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

# Create a lazy table from the drug DIM table
drug_db <- con %>%
  tbl(from = in_schema("SB_DIM", "CDR_DRUG_BNF_DIM"))

# Filter to BNF chapters 1 - 4 and 6 - 10 (inline with polypharmacy) in 
# 2020/2021 and subset columns
drug_db <- drug_db %>%
  filter(BNF_CHAPTER %in% c(01, 02, 03, 04, 06, 07, 08, 09, 10)) %>%
  inner_join(year_month_db) %>%
  select(YEAR_MONTH, RECORD_ID, CHEMICAL_SUBSTANCE_BNF_DESCR)

# Create a lazy table from the care home FACT table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))
  
# Join the drug information to the FACT table
fact_db <- fact_db %>%
  inner_join(
    y = drug_db,
    by = c("YEAR_MONTH", "CALC_PREC_DRUG_RECORD_ID" = "RECORD_ID")
  )

# Unique medicines patient dataframe
unique_medicines_db <- fact_db %>%
  mutate(CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home")) %>%
  group_by(YEAR_MONTH, CH_FLAG, NHS_NO) %>%
  summarise(UNIQUE_MEDICINES = n_distinct(CHEMICAL_SUBSTANCE_BNF_DESCR)) %>%
  ungroup()

# Number of unique medicines per patient per month by care home flag
unique_medicines_per_patient_df <- unique_medicines_db %>%
  group_by(YEAR_MONTH, CH_FLAG) %>%
  summarise(UNIQUE_MEDICINES_PER_PATIENT = mean(UNIQUE_MEDICINES)) %>%
  ungroup()

# Add overall mean and format for highcharter
unique_medicines_per_patient_df <- unique_medicines_per_patient_df %>%
  union_all(
    y = unique_medicines_per_patient_df %>%
      group_by(CH_FLAG) %>%
      summarise(
        UNIQUE_MEDICINES_PER_PATIENT = mean(UNIQUE_MEDICINES_PER_PATIENT)
      )
  ) %>%
  arrange(YEAR_MONTH) %>%
  collect() %>%
  mutate(
    YEAR_MONTH = lubridate::ym(YEAR_MONTH),
    CH_FLAG = forcats::fct_rev(CH_FLAG)
  )

## Multiple medicines per patient

# Multiple medicines per patient per month by care home flag
ten_or_more_unique_medicines_per_patient_df <- unique_medicines_db %>%
  group_by(YEAR_MONTH, CH_FLAG) %>%
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

# Add overall mean and format for highcharter
ten_or_more_unique_medicines_per_patient_df <-
  ten_or_more_unique_medicines_per_patient_df %>%
  union_all(
    y = ten_or_more_unique_medicines_per_patient_df %>%
      group_by(CH_FLAG) %>%
      summarise(
        PCT_PATIENTS_TEN_OR_MORE = mean(PCT_PATIENTS_TEN_OR_MORE)
      )
  ) %>%
  arrange(YEAR_MONTH) %>%
  collect() %>%
  mutate(
    YEAR_MONTH = lubridate::ym(YEAR_MONTH),
    CH_FLAG = forcats::fct_rev(CH_FLAG)
  )

# Add to data-raw/
usethis::use_data(unique_medicines_per_patient_df, overwrite = TRUE)
usethis::use_data(ten_or_more_unique_medicines_per_patient_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)
