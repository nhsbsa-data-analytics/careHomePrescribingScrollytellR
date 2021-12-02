library(dplyr)

# Connections and Existing Table check

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Check if the table exists
exists <- DBI::dbExistsTable(conn = con, name = "INT615_CQC_CARE_HOME")

# Drop any existing table beforehand
if (exists) DBI::dbRemoveTable(conn = con, name = "INT615_CQC_CARE_HOME")

# Pull CQC data

# Set a partner code (if we don't set this then we struggle to throttle calls)
cqcr::cqc_partner_code() # NHSBSA

# When we use the CQC API we need to be careful not to exceed 600 requests per
# minute (10 per second)

# Pull the CQC ID name and postcode for every care home (~ 60 requests)
cqc_locations_df <- cqcr::cqc_locations_search(care_home = TRUE)

# Now we need to get the details for these care homes. So we batch up our ~ 30k
# care homes into bundles of 10 and ensure we wait just over a second before 
# starting the next batch
cqc_locations_dfs <- split(cqc_locations_df, seq(nrow(cqc_locations_df)) %/% 10)
cqc_batch_details <- list()
for (batch in cqc_locations_dfs) {
  
  # Record the start time
  start <- Sys.time()
  
  # Get the batch results and append them to the existing ones
  cqc_batch_details <- c(cqc_batch_details, cqcr::cqc_location_details(batch))
  
  # Pause for the remainder of just over a second
  Sys.sleep(max(0, 1.1 - as.numeric(Sys.time() - start)))
  
}

# Process CQC data and write to DB

# Convert the batch results into a dataframe
cqc_details_df <- purrr::map_df(
  .x = cqc_batch_details, 
  .f = ~ dplyr::bind_rows(unlist(x = .x))
)

# For care homes project we are only interested in a subset of columns, so lets
# extract them
cqc_details_df <- cqc_details_df %>%
  # Filter to the period of interest
  mutate(
    # Add the nursing home / residential home flag
    nursing_home = ifelse(
      test = if_any(
        .cols = starts_with("gac") & contains("name"), 
        .fns = ~ grepl(pattern = "Nursing home", x = .x)
      ),
      yes = 1,
      no = 0
    ),
    residential_home = ifelse(
      test = if_any(
        .cols = starts_with("gac") & contains("name"), 
        .fns = ~ grepl(pattern = "Residential home", x = .x)
      ),
      yes = 1,
      no = 0
    ),
    # Change type of numeric col
    number_of_beds = as.numeric(number_of_beds)
  ) %>%
  # Select the required cols and uppercase
  select(
    location_id,
    uprn,
    registration_status,
    registration_date,
    deregistration_date,
    dormancy,
    name,
    postal_address_line_1,
    postal_address_line_2,
    postal_address_town_city,
    postal_address_county,
    postal_code,
    nursing_home,
    residential_home,
    type,
    number_of_beds
  ) %>%
  rename_with(toupper)

# Upload to DB
con %>%
  DBI::dbWriteTable(name = "INT615_CQC_CARE_HOME", value = cqc_details_df)

# Disconnect connection to database
DBI::dbDisconnect(con)
