# Load library
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Load the 5 tables required for script

# AddressBase base table
ab_base <- con %>%
  tbl(from = in_schema("ADNSH", "ADDRESSBASE_PLUS_CARE_HOME"))

# Prescription base table
presc_base <- con %>%
  tbl(from = in_schema("ADNSH", "FORM_LEVEL_CARE_HOME_FACT"))

# Determine Exact Matches function
exact_match_data = function(primary_df, lookup_df){
  
  exact_matches <- primary_df %>% 
    inner_join(
      y = lookup_df,
      by = c("PRIMARY_ADDRESS" = "LOOKUP_ADDRESS", "POSTCODE")
    ) %>% 
    # Exact match on complete address string (tokkens required for JW match)
    # NumericaL token score is *4 (to promote correct numbered address scoring)
    mutate(
      MATCH_SCORE = 1,
      MATCH_TYPE = 'EXACT',
      LOOKUP_ADDRESS = PRIMARY_ADDRESS
    ) %>% 
    # Sort Column Order for later union_all
    select(
      POSTCODE,
      PRIMARY_ID,
      PRIMARY_ADDRESS,
      LOOKUP_ID,
      LOOKUP_ADDRESS,
      MATCH_SCORE,
      MATCH_TYPE
    )
  return(exact_matches)
}

# Determine non-matches
non_match_data = function(primary_df, lookup_df){
  
  # Filter primary_df by postcodes within lookup_df
  lookup_postcodes <- lookup_df %>% 
    filter(!is.na(POSTCODE)) %>% 
    select(POSTCODE) %>% 
    distinct()
  
  # Filter primary_df by above postcodes
  non_matches <- primary_df %>% 
    anti_join(y = lookup_postcodes, by = "POSTCODE") %>% 
    mutate(
      LOOKUP_ID = NA,
      LOOKUP_ID = as.integer(LOOKUP_ID),
      LOOKUP_ADDRESS = NA,
      LOOKUP_ADDRESS = as.character(LOOKUP_ADDRESS),
      MATCH_SCORE = NA,
      MATCH_SCORE = as.integer(MATCH_SCORE),
      MATCH_TYPE = 'NONE'
    )
  return(non_matches)
}

# Determine JW Matches 
address_match_data = function(primary_df, lookup_df){
  
  # Filter primary_df by postcodes within lookup_df
  lookup_postcodes <- lookup_df %>% 
    filter(!is.na(POSTCODE)) %>% 
    select(POSTCODE) %>% 
    distinct()
  
  # Filter primary_df by above postcodes
  primary_df <- primary_df %>% 
    inner_join(y = lookup_postcodes, by = "POSTCODE")
  
  # Remove exact postcode-address matches
  primary_df <- primary_df %>% 
    anti_join(
      y = lookup_df,
      by = c("PRIMARY_ADDRESS" = "LOOKUP_ADDRESS", "POSTCODE")
      )
  
  # Unnest tokens of remaining primary_df records
  primary_tokens <- primary_df %>% 
    nhsbsaR::oracle_unnest_tokens(col = "PRIMARY_ADDRESS") %>% 
    rename(PRIMARY_ADDRESS = TOKEN) %>% 
    # Generate INT_FLAG and TOTAL to calculate later JW score
    mutate(
      INT_FLAG = ifelse(REGEXP_LIKE(PRIMARY_ADDRESS, '[0-9]'), 1, 0),
      CHAR_FLAG = ifelse(INT_FLAG == 1, 0, 1)
    ) %>% 
    group_by(PRIMARY_ID) %>% 
    mutate(
      TOTAL = (sum(INT_FLAG) * 4) + sum(CHAR_FLAG)
    ) %>% 
    ungroup() %>% 
    select(-CHAR_FLAG)
  
  # Unnest tokens from lookup_df
  lookup_tokens <- lookup_df %>% 
    nhsbsaR::oracle_unnest_tokens(col = "LOOKUP_ADDRESS") %>% 
    rename(LOOKUP_ADDRESS = TOKEN) %>% 
    # Lookup tokens can be distinct
    distinct() %>% 
    # Just INT_FLAG required for loookup_df
    mutate(INT_FLAG = ifelse(REGEXP_LIKE(LOOKUP_ADDRESS, '[0-9]'), 1, 0))
  
  # Get Token-level exact matches (thus avoiding JW as score = 1)
  cross_join_exact = primary_tokens %>% 
    dplyr::inner_join(
      y = lookup_tokens,
      by = c("POSTCODE", "PRIMARY_ADDRESS" = "LOOKUP_ADDRESS", "INT_FLAG"
      )
    ) %>% 
    # Exact numerical token match = 1 * 4 = 4 (thus no JW required)
    # Exact char token match = 1
    dplyr::mutate(
      JW = ifelse(INT_FLAG == 1, 4, 1),
      LOOKUP_ADDRESS = PRIMARY_ADDRESS
    ) %>% 
    select(
      PRIMARY_ID,
      LOOKUP_ID,
      POSTCODE,
      PRIMARY_ADDRESS,
      LOOKUP_ADDRESS,
      INT_FLAG,
      JW,
      TOTAL
    )
  
  # Retrieve non exact token-level matches to be considered for JW
  cross_join_diff <- primary_tokens %>%
    dplyr::inner_join(y = lookup_tokens, by = c("POSTCODE", "INT_FLAG")) %>% 
    filter(
      # As exact tokens matched, JW only applied where AB-token != Pat-token
      PRIMARY_ADDRESS != LOOKUP_ADDRESS,
      # Numerical tokens can only have exact match
      # So not required non-exact token matches
      INT_FLAG == 0,
      SUBSTR(LOOKUP_ADDRESS, 1, 1) == SUBSTR(PRIMARY_ADDRESS, 1, 1) |
        #Tokens share same second letter
        SUBSTR(LOOKUP_ADDRESS, 2, 1) == SUBSTR(PRIMARY_ADDRESS, 2, 1) |
        # Tokens share same last letter
        SUBSTR(LOOKUP_ADDRESS, LENGTH(LOOKUP_ADDRESS), 1)  ==  SUBSTR(PRIMARY_ADDRESS, LENGTH(PRIMARY_ADDRESS), 1) |
        # One token is a substring of the other
        INSTR(LOOKUP_ADDRESS, PRIMARY_ADDRESS) > 1 |
        INSTR(PRIMARY_ADDRESS, LOOKUP_ADDRESS) > 1
    ) %>% 
    # JW For Remaining Matches
    mutate(JW = UTL_MATCH.JARO_WINKLER(PRIMARY_ADDRESS, LOOKUP_ADDRESS)) %>% 
    # Only JW scores >= 0.8 are kept, which means
    # Strings must be fairly similar to meet threshold (hence above stipulations)
    filter(JW >= 0.8) %>% 
    select(
      PRIMARY_ID,
      LOOKUP_ID,
      POSTCODE,
      PRIMARY_ADDRESS,
      LOOKUP_ADDRESS,
      INT_FLAG,
      JW,
      TOTAL
    )
  
  # Calculate address-to-address match score using token scores 
  jw_matches <- cross_join_exact %>% 
    # Merge JW token scores with exact matches (with score of either 1 or 4)
    dplyr::union_all(cross_join_diff) %>% 
    # Max token-level Score
    # Maximum AB-token match score, per Pat-token within address
    dplyr::group_by(TOTAL, POSTCODE, PRIMARY_ID, LOOKUP_ID, PRIMARY_ADDRESS) %>% 
    dplyr::summarise(MAX_VAL = max(JW, na.rm = T)) %>% 
    dplyr::ungroup() %>% 
    # Sum of max-token-level scores for overall Address Score
    group_by(TOTAL, POSTCODE, PRIMARY_ID, LOOKUP_ID) %>% 
    summarise(JW_SCORE = sum(MAX_VAL, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(
      MATCH_SCORE = JW_SCORE / TOTAL,
      MATCH_TYPE = 'JW'
    ) %>% 
    # Get rank of scores against record Ids
    group_by(PRIMARY_ID) %>% 
    mutate(SCORE_RANK = dense_rank(desc(MATCH_SCORE))) %>% 
    ungroup() %>% 
    # Filter top (or joint top) ranked scores
    filter(SCORE_RANK == 1) %>% 
    select(
      PRIMARY_ID,
      LOOKUP_ID,
      POSTCODE,
      MATCH_SCORE,
      MATCH_TYPE
    )
  
  output <- jw_matches %>% 
    left_join(
      primary_df %>% 
        select(PRIMARY_ID, PRIMARY_ADDRESS),
      by = "PRIMARY_ID"
    ) %>% 
    left_join(
      lookup_df %>% 
        select(LOOKUP_ID, LOOKUP_ADDRESS),
      by = "LOOKUP_ID"
    ) %>% 
    select(
      POSTCODE,
      PRIMARY_ID,
      PRIMARY_ADDRESS,
      LOOKUP_ID,
      LOOKUP_ADDRESS,
      MATCH_SCORE,
      MATCH_TYPE
    )
  return(output)
}

# Overall function to combine all steps
total_match = function(df_one, df_two){
  
  output = exact_match_data(df_one, df_two) %>% 
    union_all(address_match_data(df_one, df_two)) %>% 
    union_all(non_match_data(df_one, df_two))
  return(output)
}

# Format primary_df for function input
df_one <- presc_base %>% 
  select(
    PRIMARY_ID = ADDRESS_RECORD_ID,
    POSTCODE = PAT_POSTCODE,
    PRIMARY_ADDRESS = PAT_ADDRESS
    ) %>% 
  distinct()

# Format lookup_df for function input
df_two <- ab_base %>% 
  select(
    LOOKUP_ID = UPRN_ID,
    POSTCODE =  AB_POSTCODE,
    LOOKUP_ADDRESS = AB_ADDRESS
    )

# Generate outputs
total_data = total_match(df_one, df_two)

# Write the table back to the DB (~1h 20m)
total_data %>%
  nhsbsaR::oracle_create_table(table_name = "CARE_HOME_MATCH")

# Disconnect from database
DBI::dbDisconnect(con)

#-------------------------------------------------------------------------------