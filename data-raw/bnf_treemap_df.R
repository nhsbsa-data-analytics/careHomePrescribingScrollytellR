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

# Create a lazy table to return DRUG ATTRIBUTE information from CDR_DRUG_BNF_DIM
# Only interested in
drug_db <- dplyr::tbl(
  src = con,
  from = dbplyr::sql(
    "SELECT YEAR_MONTH, RECORD_ID,
    '('||BNF_CHAPTER||') '||CHAPTER_DESCR as BNF_CHAPTER ,
    '('||BNF_SECTION||') '||SECTION_DESCR as BNF_SECTION,
    CHEMICAL_SUBSTANCE_BNF_DESCR FROM SB_DIM.CDR_DRUG_BNF_DIM"
  )
)


# Filter to 2020/2021 by joining with year_month
drug_db <- drug_db %>%
  dplyr::inner_join(year_month_db)


# Join fact and drug db
fact_db <- fact_db %>%
  dplyr::inner_join(y = drug_db, by = c(
    "YEAR_MONTH" = "YEAR_MONTH",
    "CALC_PREC_DRUG_RECORD_ID" = "RECORD_ID"
  ))

# group by bnf chapter and bnf section to get number of items and total NIC by care home and non care home
bnf_df <- fact_db %>%
  dplyr::mutate(
    CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home")
  ) %>%
  dplyr::group_by(CH_FLAG, BNF_CHAPTER, BNF_SECTION) %>%
  dplyr::summarise(
    ITEMS = dplyr::sum(ITEM_COUNT),
    NIC = dplyr::sum(ITEM_PAY_DR_NIC) * 0.01
  ) %>%
  dplyr::ungroup() %>%
  dplyr::collect()


# Separate CH and non CH as their top n BNF chapter will be different

bnf_ch <- bnf_df %>%
  dplyr::filter(CH_FLAG == "Care home") %>%
  dplyr::group_by(BNF_CHAPTER = forcats::fct_lump(BNF_CHAPTER, n = 7, w = ITEMS)) %>%
  dplyr::summarise(
    TOTAL_ITEMS_LEVEL1 = sum(ITEMS),
    TOTAL_NIC_LEVEL1 = sum(NIC)
  ) %>%
  dplyr::mutate(
    ITEM_P_LEVEL1 = TOTAL_ITEMS_LEVEL1 / sum(TOTAL_ITEMS_LEVEL1) * 100,
    # BNF Chapter level percentage
    NIC_P_LEVEL1 = TOTAL_NIC_LEVEL1 / sum(TOTAL_NIC_LEVEL1) * 100
  ) %>%
  dplyr::select(BNF_CHAPTER, ITEM_P_LEVEL1, NIC_P_LEVEL1)


# Percentage by level 2
bnf_ch_sub <- bnf_df %>%
  dplyr::filter(CH_FLAG == "Care home") %>%
  dplyr::group_by(BNF_CHAPTER, BNF_SECTION) %>%
  dplyr::summarise(
    TOTAL_ITEMS_LEVEL2 = sum(ITEMS),
    TOTAL_NIC_LEVEL2 = sum(NIC),
    .groups = "drop"
  ) %>%
  dplyr::inner_join(
    y = bnf_df %>%
      dplyr::filter(CH_FLAG == "Care home") %>%
      dplyr::group_by(BNF_CHAPTER) %>%
      dplyr::summarise(
        TOTAL_ITEMS_LEVEL1 = sum(ITEMS),
        TOTAL_NIC_LEVEL1 = sum(NIC),
        .groups = "drop"
      ),
    by = "BNF_CHAPTER"
  ) %>%
  dplyr::mutate(
    ITEM_P_LEVEL2 = TOTAL_ITEMS_LEVEL2 / TOTAL_ITEMS_LEVEL1 * 100,
    NIC_P_LEVEL2 = TOTAL_NIC_LEVEL2 / TOTAL_NIC_LEVEL1 * 100
  ) %>%
  dplyr::select(BNF_CHAPTER, BNF_SECTION, ITEM_P_LEVEL2, NIC_P_LEVEL2)


bnf_ch_final <- bnf_ch %>%
  dplyr::left_join(
    y = bnf_ch_sub,
    by = "BNF_CHAPTER"
  )


# Add to data-raw/
usethis::use_data(bnf_ch_final, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)
