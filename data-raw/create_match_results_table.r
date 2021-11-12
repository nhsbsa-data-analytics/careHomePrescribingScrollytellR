# Load library
library(magrittr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Load the 4 tables required for script

# AddressBase base table

ab_base <- dplyr::tbl(
  src = con,
  from = dplyr::sql(
    "SELECT * FROM INT615_AB_PLUS_BASE")
  )

# AddressBase tokens

ab_tokens <- dplyr::tbl(
  src = con,
  from = dplyr::sql(
    "SELECT * FROM INT615_AB_PLUS_TOKENS")
  )

# Prescribing base table

presc_base <- dplyr::tbl(
  src = con,
  from = dplyr::sql(
    "SELECT * FROM INT615_PRESC_BASE")
  )

# Prescribing tokens

presc_tokens <- dplyr::tbl(
  src = con,
  from = dplyr::sql(
    "SELECT * FROM INT615_PRESC_TOKENS")
  )

# Generate results base table

results_base <- presc_base %>% 
  # Distinct ADDRESS_RECORD_ID-level to rejoin to later
  dplyr::select(ADDRESS_RECORD_ID, PAT_POSTCODE, PAT_ADDRESS) %>% 
  dplyr::distinct() %>% 
  # All token-level information per ADDRESS_RECORD_ID
  dplyr::left_join(
    presc_tokens %>% 
      dplyr::select(ADDRESS_RECORD_ID, PAT_INT_COUNT, PAT_CHAR_COUNT, PAT_TOTAL) %>% 
      # Distinct tokens sufficient as single token can be matched multiple times
      dplyr::distinct(),
    by = "ADDRESS_RECORD_ID"
  )

# Process
# 1. Determine Exact Matches
# 2. Calculate JW Matches
# 3. Determine non-Matches
# 4. Union all results and rejoin back to results base table

# Exact Matches

exact_matches <- results_base %>% 
  dplyr::inner_join(ab_base, by = c("PAT_ADDRESS" = "AB_ADDRESS", "PAT_POSTCODE" = "AB_POSTCODE")) %>% 
  # Exact match on complete address string (tokkens required for JW match)
  dplyr::mutate(MATCH_SCORE = 1) %>% 
  dplyr::mutate(MATCH_TYPE = 'EXACT') %>% 
  # NumericAL token score is *4 (to promote correct numbered address, when number present, to have higher scores)
  dplyr::mutate(JW_SCORE = (PAT_INT_COUNT * 4) + PAT_CHAR_COUNT) %>% 
  dplyr::mutate(TOTAL_SCORE = (PAT_INT_COUNT * 4) + PAT_CHAR_COUNT) %>% 
  # Sort Column Order for later union_all
  dplyr::select(UPRN, UPRN_ID, CLASS, CAREHOME_FLAG, AB_ADDRESS = PAT_ADDRESS, ADDRESS_RECORD_ID, JW_SCORE, TOTAL_SCORE, MATCH_SCORE, MATCH_TYPE)

# Remaining Presc Tokens to carry forward into JW Matching

pat_tokens <- presc_tokens %>% 
  dplyr::distinct() %>% 
  # Tokens from Exact match records not needed to be considered
  dplyr::anti_join(exact_matches, by = "ADDRESS_RECORD_ID")

# Remaining Plus Tokens to carry forward into JW Matching

plus_tokens <- ab_tokens %>% 
  dplyr::select(UPRN, UPRN_ID, AB_POSTCODE, AB_ADDRESS, INT_FLAG) %>% 
  # AddressBase and Presc tokens joined, in order to be compared and scored against each other
  dplyr::inner_join(
    pat_tokens %>% 
      dplyr::select(PAT_POSTCODE) %>% 
      dplyr::distinct(),
    # Postcode level join, as records only matched against AB addresses within same postcode
    by = c("AB_POSTCODE" = "PAT_POSTCODE")
  )

# Get Token-level exact matches (thus avoiding JW as score = 1)

cross_join_exact = pat_tokens %>% 
  dplyr::select(ADDRESS_RECORD_ID, PAT_POSTCODE, PAT_ADDRESS, INT_FLAG) %>% 
  # Exact numerical token match = 1 * 4 = 4 (thus no JW required) - char token match = 1
  dplyr::mutate(JW = ifelse(INT_FLAG == 1, 4, 1)) %>% 
  dplyr::inner_join(
    # Rejoin to get accompanying token information
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
  # As exact tokens already matched, JW only need to be applied where AB-token != Pat-token
  dplyr::filter(AB_ADDRESS != PAT_ADDRESS) %>% 
  # Numerical tokens can only have exact match, so not required non-exact token matches
  dplyr::filter(INT_FLAG == 0) %>% 
  # The goal here is to limit the JW calculation (consumes resource) to only potential matches
  dplyr::filter(
    # Tokens share same first letter
    SUBSTR(AB_ADDRESS, 1, 1) == SUBSTR(PAT_ADDRESS, 1, 1) |
      #Tokens share same second letter
      SUBSTR(AB_ADDRESS, 2, 1) == SUBSTR(PAT_ADDRESS, 2, 1) |
      # Tokens share same last letter
      SUBSTR(AB_ADDRESS, LENGTH(AB_ADDRESS), 1)  ==  SUBSTR(PAT_ADDRESS, LENGTH(PAT_ADDRESS), 1) |
      # One token is a substring of the other
      INSTR(AB_ADDRESS, PAT_ADDRESS) > 1 |
      INSTR(PAT_ADDRESS, AB_ADDRESS) > 1
  ) %>% 
  # JW For Remaining Matches
  dplyr::mutate(JW = UTL_MATCH.JARO_WINKLER(PAT_ADDRESS, AB_ADDRESS)) %>% 
  # Only JW scores >= 0.8 are kept, which means strings must be fairly similar to meet threshold (hence above stipulations)
  dplyr::filter(JW >= 0.8) %>% 
  dplyr::select(UPRN, UPRN_ID, ADDRESS_RECORD_ID, PAT_POSTCODE, AB_ADDRESS, PAT_ADDRESS, INT_FLAG, JW) %>% 
  # Merge JW token scores with exact matches (with score of either 1 or 4)
  dplyr::union_all(cross_join_exact) %>% 
  # Max token-level Score
  dplyr::group_by(UPRN, UPRN_ID, ADDRESS_RECORD_ID, PAT_ADDRESS) %>% 
  # Maximum AB-token match score, per Pat-token within address
  dplyr::summarise(MAX_VAL = max(JW, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  # Sum of max-token-level scores for overall Address Score
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
  # Match score is actual score / maximum potential score
  dplyr::mutate(MATCH_SCORE = JW_SCORE / TOTAL_SCORE)

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
  # This means single record is chosen, where 1 UPRN yet 2-3 UPRN_ID are joint best match
  dplyr::filter(UPRN_COUNT == 1 & UPRN_ID_RANK == 1) %>% 
  # Manually add in Match Type info
  dplyr::mutate(MATCH_TYPE = 'JW') %>% 
  # Rejoin for some additional AB Data
  dplyr::inner_join(
    ab_base %>% 
      dplyr::select(CLASS, CAREHOME_FLAG, AB_ADDRESS, UPRN_ID),
    by = "UPRN_ID"
  ) %>% 
  # Sort Column Order for later union_all
  dplyr::select(UPRN, UPRN_ID, CLASS, CAREHOME_FLAG, AB_ADDRESS, ADDRESS_RECORD_ID, JW_SCORE, TOTAL_SCORE, MATCH_SCORE, MATCH_TYPE)

# Non-Matches

non_matches = results_base %>% 
  # If record is not JW or exact match, is therefore a non-match
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
  # Sort Column TYpes for generated empty fields - Important otherwise data types affect union_all later on
  dplyr::mutate(across(c("UPRN", "UPRN_ID", "CAREHOME_FLAG"), as.integer)) %>% 
  dplyr::mutate(across(c("CLASS", "AB_ADDRESS"), as.character)) %>% 
  # Sort Column Order for later union_all
  dplyr::select(UPRN, UPRN_ID, CLASS, CAREHOME_FLAG, AB_ADDRESS, ADDRESS_RECORD_ID, JW_SCORE, TOTAL_SCORE, MATCH_SCORE, MATCH_TYPE)

# Matching Results

results = exact_matches %>% 
  dplyr::union_all(jw_matches) %>% 
  dplyr::union_all(non_matches) %>% 
  # After union_all 3 datasets, join back to results_base
  dplyr::inner_join(results_base, by = "ADDRESS_RECORD_ID") %>% 
  # Sort Column Order for later union_all
  dplyr::select(
    ADDRESS_RECORD_ID, PAT_POSTCODE, PAT_ADDRESS, PAT_INT_COUNT, PAT_CHAR_COUNT, PAT_TOTAL,
    UPRN, UPRN_ID, CLASS, CAREHOME_FLAG, AB_ADDRESS, ADDRESS_RECORD_ID, JW_SCORE, TOTAL_SCORE, MATCH_SCORE, MATCH_TYPE
    )

# Get PF_ID of matched records

# Local Final Results

results_df = results %>% 
  dplyr::collect()

# Disconnect

DBI::dbDisconnect(con)
rm(con)

#-------------------------------------------------------------------------------