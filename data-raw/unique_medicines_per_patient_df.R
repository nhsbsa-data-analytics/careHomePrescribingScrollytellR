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
  dplyr::select(YEAR_MONTH, RECORD_ID, CHEMICAL_SUBSTANCE_BNF_DESCR, BNF_CHAPTER)

# Join the drug information to the FACT table
fact_db <- fact_db %>%
  dplyr::inner_join(
    y = drug_db %>%
      dplyr::select(YEAR_MONTH, RECORD_ID, CHEMICAL_SUBSTANCE_BNF_DESCR, BNF_CHAPTER),
    by = c(
      "YEAR_MONTH" = "YEAR_MONTH",
      "CALC_PREC_DRUG_RECORD_ID" = "RECORD_ID"
    )
  )

# Unique medicines patient dataframe BNF_CHAPTER 1:10
unique_medicines_db <- fact_db %>%
  dplyr::filter(BNF_CHAPTER %in% c(01, 02, 03, 04, 05, 06, 07, 08, 09, 10)) %>%
  dplyr::mutate(
    CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home")
  ) %>%
  dplyr::group_by(YEAR_MONTH, CH_FLAG, NHS_NO) %>%
  dplyr::summarise(
    UNIQUE_MEDICINES = dplyr::n_distinct(CHEMICAL_SUBSTANCE_BNF_DESCR)
  ) %>%
  dplyr::ungroup()

# Number of unique medicines per patient per month by care home flag
unique_medicines_per_patient_df <- unique_medicines_db %>%
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

## Multiple medicines per patient

# Multiple medicines per patient per month by care home flag
ten_or_more_unique_medicines_per_patient_df <- unique_medicines_db %>%
  dplyr::group_by(YEAR_MONTH, CH_FLAG) %>%
  dplyr::summarise(
    PATIENTS_TEN_OR_MORE = dplyr::n_distinct(
      ifelse(test = UNIQUE_MEDICINES >= 10,
        yes = NHS_NO,
        no = NA
      )
    ),
    TOTAL_PATIENTS = dplyr::n_distinct(NHS_NO)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    PCT_PATIENTS_TEN_OR_MORE = PATIENTS_TEN_OR_MORE / TOTAL_PATIENTS * 100
  ) %>%
  dplyr::select(-c(PATIENTS_TEN_OR_MORE, TOTAL_PATIENTS))

# Add overall mean and format for highcharter
ten_or_more_unique_medicines_per_patient_df <-
  ten_or_more_unique_medicines_per_patient_df %>%
  dplyr::union_all(
    y = ten_or_more_unique_medicines_per_patient_df %>%
      dplyr::group_by(CH_FLAG) %>%
      dplyr::summarise(
        PCT_PATIENTS_TEN_OR_MORE = mean(PCT_PATIENTS_TEN_OR_MORE)
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
usethis::use_data(ten_or_more_unique_medicines_per_patient_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)
