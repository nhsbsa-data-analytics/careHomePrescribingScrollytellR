# load library
library(magrittr)

# Set up connection to the DB
# database can be "DALP" or "DWCP"
con <- nhsbsaR::con_nhsbsa(database = "DWCP")

# Create a lazy table from the carehome pfid lookup
care_home_df <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM DALL_REF.INT615_CAREHOME_PFID_LKP")
)

# create a lazy table from the PX FACT table
# filter to remove unwanted prescriptions from PX FACT table
px_data_df <-
  dbplyr::sql("select * from AML.PX_FORM_ITEM_ELEM_COMB_FACT@dwcpb") %>%
  dplyr::tbl(src = con, from = .) %>%
  dplyr::filter(
    PAY_DA_END == "N", # excludes disallowed items
    PAY_ND_END == "N", # excludes not dispensed items
    PAY_RB_END == "N", # excludes referred back items
    CD_REQ == "N", # excludes controlled drug requisitions
    OOHC_IND == 0, # excludes out of hours dispensing
    PRIVATE_IND == 0, # excludes private dispensers
    IGNORE_FLAG == "N" # excludes LDP dummy forms
  )

# attempt to join carehome lookup against FACT PX data
df_join <- px_data_df %>%
  dplyr::left_join(y = care_home_df, by = c("YEAR_MONTH", "PF_ID")) %>%
  dplyr::filter(YEAR_MONTH == 202101) %>%
  dplyr::select(
    YEAR_MONTH,
    PF_ID,
    ITEM_COUNT,
    ITEM_PAY_DR_NIC,
    CAREHOME_MATCH
  )


## SUM(fact.ITEM_COUNT) as ITEM_COUNT,
## SUM(fact.ITEM_PAY_DR_NIC) * 0.01 as ITEM_PAY_DR_NIC,
## COUNT(DISTINCT PF_ID) as FORM_COUNT


# write out final output table to database


DBI::dbDisconnect(con)
