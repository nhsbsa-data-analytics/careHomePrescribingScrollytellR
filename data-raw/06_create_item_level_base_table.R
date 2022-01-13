library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Check if the table exists
exists <- DBI::dbExistsTable(
  conn = con, 
  name = "INT615_ITEM_LEVEL_BASE"
)

# Drop any existing table beforehand
if (exists) {
  DBI::dbRemoveTable(
    conn = con, 
    name = "INT615_ITEM_LEVEL_BASE"
  )
}

# Initial lazy tables from database

# Create a lazy table from the item level FACT table
item_fact_db <- con %>%
  tbl(from = in_schema("AML", "PX_FORM_ITEM_ELEM_COMB_FACT"))

# Create a lazy table from the item level FACT table
form_fact_db <- con %>%
  tbl(from = "INT615_FORM_LEVEL_FACT")

# Create a lazy table from the matched patient address care home table
patient_address_match_db <- con %>%
  tbl(from = "INT615_ADDRESS_MATCHED")

# Create a lazy table from the geography lookup table (Region, STP and LA)
postcode_db <- con %>%
  tbl(from = "INT615_POSTCODE_LOOKUP")

# Create a lazy table from the drug DIM table
drug_db <- con %>%
  tbl(from = in_schema("DIM", "CDR_EP_DRUG_BNF_DIM"))

# Create item level FACT table

# Filter to elderly patients in 2020/2021 and required columns
item_fact_db <- item_fact_db %>%
  filter(
    # Elderly patients identified patients in the period
    CALC_AGE >= 65L,
    PATIENT_IDENTIFIED == "Y",
    YEAR_MONTH >= 202004L & YEAR_MONTH <= 202103L,
    # Standard exclusions
    PAY_DA_END == "N", # excludes disallowed items
    PAY_ND_END == "N", # excludes not dispensed items
    PAY_RB_END == "N", # excludes referred back items
    CD_REQ == "N", # excludes controlled drug requisitions 
    OOHC_IND == 0L, # excludes out of hours dispensing
    PRIVATE_IND == 0L, # excludes private dispensers
    IGNORE_FLAG == "N" # excludes LDP dummy forms
  ) %>%
  select(
    YEAR_MONTH,
    PF_ID,
    EPS_FLAG,
    PART_DATE = EPS_PART_DATE,
    EPM_ID,
    PDS_GENDER,
    CALC_AGE,
    PATIENT_IDENTIFIED,
    NHS_NO,
    CALC_PREC_DRUG_RECORD_ID,
    ITEM_COUNT,
    ITEM_PAY_DR_NIC,
    ITEM_CALC_PAY_QTY
  )

# Now we join the columns of interest back to the fact table and fill the 
# care home flag and match type columns
item_fact_db <- item_fact_db %>%
  inner_join(
    y = form_fact_db,
    na_matches = "na", # Match NA to NA for PART_DATE and EPM_ID
    copy = TRUE
  ) %>%
  left_join(
    y = patient_address_match_db %>% 
      select(-c(MONTHS_5PLUS_PATIENTS, MAX_MONTHLY_PATIENTS)),
    copy = TRUE
  ) %>%
  tidyr::replace_na(list(CH_FLAG = 0L, MATCH_TYPE = "NO MATCH"))

# Tidy care home flag and join the postcode info
item_fact_db <- item_fact_db %>%
  mutate(CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home")) %>%
  left_join(
    y = postcode_db,
    copy = TRUE
  ) %>%
  relocate(PCD_REGION_CODE:IMD_QUINTILE, .after = POSTCODE)

# Add the drug information
item_fact_db <- item_fact_db %>%
  left_join(
    y = drug_db %>%
      filter(YEAR_MONTH >= 202004L & YEAR_MONTH <= 202103L) %>%
      select(
        YEAR_MONTH,
        CALC_PREC_DRUG_RECORD_ID = RECORD_ID,
        BNF_CHAPTER,
        CHAPTER_DESCR,
        BNF_SECTION,
        SECTION_DESCR,
        BNF_PARAGRAPH,
        PARAGRAPH_DESCR,
        BNF_CHEMICAL_SUBSTANCE,
        CHEMICAL_SUBSTANCE_BNF_DESCR
      ),
    copy = TRUE
  ) %>%
  relocate(
    BNF_CHAPTER:CHEMICAL_SUBSTANCE_BNF_DESCR, 
    .before = CALC_PREC_DRUG_RECORD_ID
  )

# Get a single gender and age for the period
patient_db <- item_fact_db %>%
  group_by(NHS_NO) %>%
  summarise(
    # Gender
    MALE_COUNT = sum(
      ifelse(PDS_GENDER == 1, 1, 0),
      na.rm = TRUE
    ),
    FEMALE_COUNT = sum(
      ifelse(PDS_GENDER == 2, 1, 0),
      na.rm = TRUE
    ),
    # Take the max age
    AGE = max(
      CALC_AGE,
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  mutate(
    GENDER = case_when(
      MALE_COUNT > 0 & FEMALE_COUNT == 0 ~ "Male",
      MALE_COUNT == 0 & FEMALE_COUNT > 0 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-ends_with("_COUNT")) %>%
  # Add an age band
  mutate(
    AGE_BAND = case_when(
      AGE < 70 ~ "65-69",
      AGE < 75 ~ "70-74",
      AGE < 80 ~ "75-79",
      AGE < 85 ~ "80-84",
      AGE < 90 ~ "85-89",
      TRUE ~ "90+"
    )
  )

# Join fact data to patient level dimension
item_fact_db <- item_fact_db %>%
  left_join(
    y = patient_db,
    copy = TRUE
  ) %>%
  relocate(GENDER, .after = PDS_GENDER) %>%
  relocate(AGE_BAND, AGE, .after = CALC_AGE) %>%
  select(-c(PDS_GENDER, CALC_AGE))

# Write the table back to the DB
item_fact_db %>%
  nhsbsaR::oracle_create_table(
    table_name = "INT615_ITEM_LEVEL_BASE"
  )

# Disconnect from database
DBI::dbDisconnect(con)
