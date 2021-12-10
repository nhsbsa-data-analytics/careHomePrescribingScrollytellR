library(dplyr)

# All BUC boundary files @ CRS 27700
map_df <- bind_rows(
  
  # Region
  sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Regions_December_2020_EN_BUC_V2/FeatureServer/0/query?where=1%3D1&outFields=RGN20CD,RGN20NM&outSR=27700&f=json") %>%
    mutate(GEOGRAPHY = "Region") %>%
    dplyr::select(
      GEOGRAPHY,
      SUB_GEOGRAPHY_CODE = RGN20CD,
      SUB_GEOGRAPHY_NAME = RGN20NM,
      GEOMETRY = geometry
    ),
  
  # STP
  sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Sustainability_and_Transformation_Partnerships_April_2021_EN_BUC_v2/FeatureServer/0/query?where=1%3D1&outFields=STP21CD,STP21NM&outSR=27700&f=json") %>%
    mutate(GEOGRAPHY = "STP") %>%
    dplyr::select(
      GEOGRAPHY,
      SUB_GEOGRAPHY_CODE = STP21CD,
      SUB_GEOGRAPHY_NAME = STP21NM,
      GEOMETRY = geometry
    ),
  
  # Local Authority
  sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LocalAuthorityDistrictsMay2021UKBUC/FeatureServer/0/query?where=1%3D1&outFields=LAD21CD,LAD21NM&outSR=27700&f=json") %>%
    mutate(GEOGRAPHY = "Local Authority") %>%
    dplyr::select(
      GEOGRAPHY,
      SUB_GEOGRAPHY_CODE = LAD21CD,
      SUB_GEOGRAPHY_NAME = LAD21NM,
      GEOMETRY = geometry
    )
  
)

# Add to data-raw/
usethis::use_data(map_df, overwrite = TRUE)
