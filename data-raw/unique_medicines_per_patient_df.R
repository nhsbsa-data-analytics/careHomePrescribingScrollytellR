# Load library
library(magrittr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the care home FACT table
fact_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM DALL_REF.INT615_ITEM_LEVEL_BASE")
)

# Create a laxy table to return DRUG ATTRIBUTE information from CDR_DRUG_BNF_DIM
drug_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql(
    "SELECT * FROM SB_DIM.CDR_DRUG_BNF_DIM
  WHERE 1=1
  AND YEAR_MONTH in
  (202004, 202005, 202006, 202007, 202008, 202009,
  202010, 202011, 202012, 202101, 202102, 202103)"
  )
)

# Join fact and drug db
fact_drug_db <- fact_db %>%
  dplyr::inner_join(drug_db, by = c(
    "CALC_PREC_DRUG_RECORD_ID" = "RECORD_ID",
    "YEAR_MONTH" = "YEAR_MONTH"
  ))

# Monthly number of unique medicines per patient by care home flag
uniq_med_per_patient_df <- fact_drug_db %>%
  dplyr::mutate(
    CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home")
  ) %>%
  dplyr::group_by(YEAR_MONTH, CH_FLAG, NHS_NO) %>%
  dplyr::summarise(
    UNIQ_CHEM_SUB = dplyr::n_distinct(CHEMICAL_SUBSTANCE_BNF_DESCR)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(CH_FLAG) %>%
  dplyr::summarise(
    "Unique medicines per patient, per month" = dplyr::round(mean(UNIQ_CHEM_SUB), 0)
  ) %>%
  dplyr::collect()

# Add to data-raw/
usethis::use_data(uniq_med_per_patient_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)
