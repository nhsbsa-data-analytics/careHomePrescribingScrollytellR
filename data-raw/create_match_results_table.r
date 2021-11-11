# Load library
library(magrittr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Load the 4 tables required for script

ab_base <- dplyr::tbl(
  src = con,
  from = dplyr::sql(
    "SELECT * FROM INT615_AB_PLUS_BASE")
  )

ab_tokens <- dplyr::tbl(
  src = con,
  from = dplyr::sql(
    "SELECT * FROM INT615_AB_PLUS_TOKENS")
  )  

presc_base <- dplyr::tbl(
  src = con,
  from = dplyr::sql(
    "SELECT * FROM INT615_PRESC_BASE")
  )

presc_tokens <- dplyr::tbl(
  src = con,
  from = dplyr::sql(
    "SELECT * FROM INT615_PRESC_TOKENS")
  )

# Generate results base table

results_base <- presc_base %>% 
  dplyr::select(ADDRESS_RECORD_ID, PAT_POSTCODE, PAT_ADDRESS) %>% 
  dplyr::distinct() %>% 
  dplyr::left_join(
    presc_tokens %>% 
      dplyr::select(ADDRESS_RECORD_ID, PAT_INT_COUNT, PAT_CHAR_COUNT, PAT_TOTAL) %>% 
      dplyr::distinct(),
    by = "ADDRESS_RECORD_ID"
  )

# Exact Matches

exact_matches <- results_base %>% 
  dplyr::inner_join(ab_base, by = c("PAT_ADDRESS" = "AB_ADDRESS", "PAT_POSTCODE" = "AB_POSTCODE")) %>% 
  dplyr::mutate(MATCH_SCORE = 1) %>% 
  dplyr::mutate(MATCH_TYPE = 'EXACT') %>% 
  dplyr::mutate(JW_SCORE = (PAT_INT_COUNT * 4) + PAT_CHAR_COUNT) %>% 
  dplyr::mutate(TOTAL_SCORE = (PAT_INT_COUNT * 4) + PAT_CHAR_COUNT) %>% 
  dplyr::select(UPRN, UPRN_ID, CLASS, CAREHOME_FLAG, AB_ADDRESS = PAT_ADDRESS, ADDRESS_RECORD_ID, JW_SCORE, TOTAL_SCORE, MATCH_SCORE, MATCH_TYPE)

# Remaining Presc Tokens to carry forward into JW Matching

pat_tokens <- presc_tokens %>% 
  dplyr::distinct() %>% 
  dplyr::anti_join(exact_matches, by = "ADDRESS_RECORD_ID")

# Remaining Plus Tokens to carry forward into JW Matching

plus_tokens <- ab_tokens %>% 
  dplyr::select(UPRN, UPRN_ID, AB_POSTCODE, AB_ADDRESS, INT_FLAG) %>% 
  dplyr::inner_join(
    pat_tokens %>% 
      dplyr::select(PAT_POSTCODE) %>% 
      dplyr::distinct(),
    by = c("AB_POSTCODE" = "PAT_POSTCODE")
  )

# Get Token-level exact matches (thus avoiding JW as score = 1)

cross_join_exact = pat_tokens %>% 
  dplyr::select(ADDRESS_RECORD_ID, PAT_POSTCODE, PAT_ADDRESS, INT_FLAG) %>% 
  dplyr::mutate(JW = ifelse(INT_FLAG == 1, 4, 1)) %>% 
  dplyr::inner_join(
    plus_tokens %>% 
      dplyr::select(UPRN, UPRN_ID, AB_ADDRESS, INT_FLAG, AB_POSTCODE),
    by = c("PAT_POSTCODE" = "AB_POSTCODE", "PAT_ADDRESS" = "AB_ADDRESS", "INT_FLAG" = "INT_FLAG")
  ) %>% 
  dplyr::mutate(AB_ADDRESS = PAT_ADDRESS) %>% 
  dplyr::select(UPRN, UPRN_ID, ADDRESS_RECORD_ID, PAT_POSTCODE, AB_ADDRESS, PAT_ADDRESS, INT_FLAG, JW)

# Retrieve non exact token-level matches to be considered for JW

jw_union <- pat_tokens %>% 
  dplyr::select(ADDRESS_RECORD_ID, PAT_POSTCODE, PAT_ADDRESS, INT_FLAG) %>% 
  dplyr::inner_join(
    plus_tokens %>% 
      dplyr::select(UPRN, UPRN_ID, AB_ADDRESS, INT_FLAG, AB_POSTCODE),
    by = c("PAT_POSTCODE" = "AB_POSTCODE", "INT_FLAG" = "INT_FLAG")
  ) %>% 
  dplyr::filter(AB_ADDRESS != PAT_ADDRESS) %>% 
  dplyr::filter(INT_FLAG == 0) %>% 
  dplyr::filter(
    SUBSTR(AB_ADDRESS, 1, 1) == SUBSTR(PAT_ADDRESS, 1, 1) |
      SUBSTR(AB_ADDRESS, 2, 1) == SUBSTR(PAT_ADDRESS, 2, 1) |
      SUBSTR(AB_ADDRESS, LENGTH(AB_ADDRESS), 1)  ==  SUBSTR(PAT_ADDRESS, LENGTH(PAT_ADDRESS), 1) |
      INSTR(AB_ADDRESS, PAT_ADDRESS) > 1 |
      INSTR(PAT_ADDRESS, AB_ADDRESS) > 1
  ) %>% 
  # JW For Remaining Matches
  dplyr::mutate(JW = UTL_MATCH.JARO_WINKLER(PAT_ADDRESS, AB_ADDRESS)) %>% 
  dplyr::filter(JW >= 0.8) %>% 
  dplyr::select(UPRN, UPRN_ID, ADDRESS_RECORD_ID, PAT_POSTCODE, AB_ADDRESS, PAT_ADDRESS, INT_FLAG, JW) %>% 
  dplyr::union_all(cross_join_exact) %>% 
  # Max token-level Score
  dplyr::group_by(UPRN, UPRN_ID, ADDRESS_RECORD_ID, PAT_ADDRESS) %>% 
  dplyr::summarise(MAX_VAL = max(JW, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  # Sum of scores for Address Score
  dplyr::group_by(UPRN, UPRN_ID, ADDRESS_RECORD_ID) %>% 
  dplyr::summarise(JW_SCORE = sum(MAX_VAL, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  # Join Token-level Info
  dplyr::inner_join(
    pat_tokens %>% 
      dplyr::select(ADDRESS_RECORD_ID, PAT_POSTCODE, PAT_CHAR_COUNT, PAT_INT_COUNT, PAT_TOTAL) %>% 
      dplyr::distinct(),
    by = "ADDRESS_RECORD_ID"
  ) %>% 
  # Calculate Score from token char/int Info
  dplyr::mutate(TOTAL_SCORE = (PAT_INT_COUNT * 4) + PAT_CHAR_COUNT) %>% 
  dplyr::mutate(MATCH_SCORE = JW_SCORE / TOTAL_SCORE)

jw_union_df = jw_union %>% 
  dplyr::collect()

# Final aggregations for JW Match Info

jw_matches = jw_union %>% 
  # Then Max Score out of all Match Scores
  dplyr::group_by(ADDRESS_RECORD_ID) %>% 
  dplyr::summarise(MATCH_SCORE = max(MATCH_SCORE, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  # Rejoin back to table with all ADDRESS_RECORD_ID and UPRN info
  dplyr::inner_join(
    jw_union,
    by = c("ADDRESS_RECORD_ID", "MATCH_SCORE")
  ) %>% 
  # Count UPRN per ADDRESS_RECORD_ID-match and UPRN_ID_RANK (to filter single UPRN_ID)
  dplyr::group_by(ADDRESS_RECORD_ID) %>% 
  dplyr::mutate(
    UPRN_ID_RANK = rank(UPRN_ID),
    UPRN_COUNT = n_distinct(UPRN)
    ) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(UPRN_COUNT == 1 & UPRN_ID_RANK == 1) %>% 
  # Manually add in Match Type info
  dplyr::mutate(MATCH_TYPE = 'JW') %>% 
  # Rejoin for some additional AB Data
  dplyr::inner_join(
    ab_base %>% 
      dplyr::select(CLASS, CAREHOME_FLAG, AB_ADDRESS, UPRN_ID),
    by = "UPRN_ID"
  ) %>% 
  # Sort Column Order
  dplyr::select(UPRN, UPRN_ID, CLASS, CAREHOME_FLAG, AB_ADDRESS, ADDRESS_RECORD_ID, JW_SCORE, TOTAL_SCORE, MATCH_SCORE, MATCH_TYPE)

jw_matches %>% 
  dplyr::mutate(n = 1) %>% 
  dplyr::summarise(n=sum(n))

# Non-Matches

non_matches = results_base %>% 
  dplyr::anti_join(exact_matches, by = "ADDRESS_RECORD_ID") %>% 
  dplyr::anti_join(jw_matches, by = "ADDRESS_RECORD_ID") %>% 
  # Generate NULL for empty fields
  dplyr::mutate(UPRN = NA) %>% 
  dplyr::mutate(UPRN_ID = NA) %>% 
  dplyr::mutate(CLASS = NA) %>% 
  dplyr::mutate(CAREHOME_FLAG = NA) %>% 
  dplyr::mutate(AB_ADDRESS = NA) %>% 
  dplyr::mutate(JW_SCORE = 0) %>% 
  dplyr::mutate(TOTAL_SCORE = 0) %>% 
  dplyr::mutate(MATCH_SCORE = 0) %>% 
  dplyr::mutate(MATCH_TYPE = 'NONE') %>% 
  # Sort Column Order
  dplyr::select(UPRN, UPRN_ID, CLASS, CAREHOME_FLAG, AB_ADDRESS, ADDRESS_RECORD_ID, JW_SCORE, TOTAL_SCORE, MATCH_SCORE, MATCH_TYPE)

# Final Results

pcd_ch_results = exact_matches %>% 
  dplyr::union_all(jw_matches) %>% 
  dplyr::union_all(non_matches) %>% 
  dplyr::inner_join(results_base, by = "ADDRESS_RECORD_ID") %>% 
  # Sort Column Order
  dplyr::select(
    ADDRESS_RECORD_ID, PAT_POSTCODE, PAT_ADDRESS, PAT_INT_COUNT, PAT_CHAR_COUNT, PAT_TOTAL,
    UPRN, UPRN_ID, CLASS, CAREHOME_FLAG, AB_ADDRESS, ADDRESS_RECORD_ID, JW_SCORE, TOTAL_SCORE, MATCH_SCORE, MATCH_TYPE
  )

# Collect Results

Sys.time()

rb = results_base %>% 
  dplyr::collect()

df = pcd_ch_results %>% 
  dplyr::collect()

Sys.time()

z = df %>% 
  dplyr::select(-c(UPRN_ID, AB_ADDRESS)) %>% 
  dplyr::distinct()

z = df %>% 
  dplyr::mutate(n = 1) %>% 
  dplyr::group_by(ADDRESS_RECORD_ID) %>% 
  dplyr::mutate(ADD_COUNT = sum(n))
  dplyr::count(ADDRESS_RECORD_ID) %>% 
  dplyr::filter(n > 1)

df %>% 
  dplyr::count(MATCH_TYPE)

rb %>% 
  dplyr::select(ADDRESS_RECORD_ID) %>% 
  dplyr::distinct()
  
# Disconnect

DBI::dbDisconnect()
rm(con)

#-------------------------------------------------------------------------------