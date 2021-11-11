# Load library
library(magrittr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the year month table
year_month_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM DALL_REF.YEAR_MONTH_DIM")
)

# Filter to 2020/2021
year_month_db <- year_month_db %>%
  dplyr::filter(FINANCIAL_YEAR == "2020/2021") %>%
  dplyr::select(YEAR_MONTH)

# Create a lazy table from the care home FACT table
fact_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM DALL_REF.INT615_ITEM_LEVEL_BASE")
)

# Create a lazy table from the drug DIM table
drug_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM SB_DIM.CDR_DRUG_BNF_DIM")
)

# Filter to 2020/2021 and subset columns
drug_db <- drug_db %>%
  dplyr::inner_join(year_month_db) %>%
  dplyr::select(YEAR_MONTH, RECORD_ID, CHEMICAL_SUBSTANCE_BNF_DESCR)

# Join the drug information to the FACT table
fact_db <- fact_db %>%
  dplyr::inner_join(
    y = drug_db %>%
      dplyr::select(YEAR_MONTH, RECORD_ID, CHEMICAL_SUBSTANCE_BNF_DESCR),
    by = c(
      "YEAR_MONTH" = "YEAR_MONTH",
      "CALC_PREC_DRUG_RECORD_ID" = "RECORD_ID"
    )
  )

# Number of unique medicines per patient per month by care home flag
unique_medicines_per_patient_df <- fact_db %>%
  dplyr::mutate(
    CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home")
  ) %>%
  dplyr::group_by(YEAR_MONTH, CH_FLAG, NHS_NO) %>%
  dplyr::summarise(
    UNIQUE_MEDICINES = dplyr::n_distinct(CHEMICAL_SUBSTANCE_BNF_DESCR)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(YEAR_MONTH, CH_FLAG) %>%
  dplyr::summarise(
    UNIQUE_MEDICINES_PER_PATIENT = mean(UNIQUE_MEDICINES)
  ) %>%
  dplyr::ungroup()

# Add overall mean and format for highcharter
unique_medicines_per_patient_df <- unique_medicines_per_patient_df %>%
  dplyr::union_all(
    y = unique_medicines_per_patient_df %>%
      dplyr::group_by(CH_FLAG) %>%
      dplyr::summarise(
        UNIQUE_MEDICINES_PER_PATIENT = mean(UNIQUE_MEDICINES_PER_PATIENT)
      )
  ) %>%
  dplyr::arrange(YEAR_MONTH) %>%
  dplyr::collect() %>%
  dplyr::mutate(
    YEAR_MONTH = lubridate::ym(YEAR_MONTH),
    CH_FLAG = forcats::fct_rev(CH_FLAG)
  )

# Add to data-raw/
usethis::use_data(unique_medicines_per_patient_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)
