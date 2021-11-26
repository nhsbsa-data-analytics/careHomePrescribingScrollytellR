#' Custom NHSBSA highcharter theme
#'
#' Based on the nhsbsaR highcharter theme, since it returns a list we can edit
#' it to the specific theme for this shiny app.
#'
#' @param palette Which colour palette to use from the `nhsbsaR` package.
#' @param stack Stack option for highcharter.
#'
#' @return
#' @export
theme_nhsbsa <- function(hc, palette = NA, stack = "normal") {

  # Load theme from nhsbsaR package
  theme_nhsbsa_hc <- nhsbsaR::theme_nhsbsa_hc(family = "Frutiger W01")

  # Add the plot options
  theme_nhsbsa_hc$plotOptions <- list(
    series = list(stacking = stack, borderWidth = 0),
    bar = list(groupPadding = 0.1)
  )

  # Add the palettes
  theme_nhsbsa_hc$colors <- nhsbsaR::palette_nhsbsa(palette = palette)
  theme_nhsbsa_hc$colAxis <- list(
    min = 0,
    minColor = nhsbsaR::palette_nhsbsa(palette = "gradient")[1],
    maxColor = nhsbsaR::palette_nhsbsa(palette = "gradient")[2]
  )

  # Add the theme to the chart and then remove the credits afterwards (currently
  # does not work to do this within the theme)
  hc %>%
    highcharter::hc_add_theme(hc_thm = theme_nhsbsa_hc) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_yAxis(title = list(text = "")) %>%
    highcharter::hc_credits(enabled = TRUE)
}


#' Add a care home flag
#'
#' Add a care home flag to the data
#'
#' @param df 
#' @param address_col
#' @param use_addressbase_plus_class_col
#'
#' @return
#' @export
calc_care_home_flag <- function(
  df, 
  address_col, 
  use_addressbase_plus_class_col=FALSE
) {
  
  df <- df %>%
  
    # Regex check for care home / non care home words (as the class isn't always
    # perfect)
    dplyr::mutate(
      # If it has this in the name then it is probably a care home
      # CARE-HOME / CARE HOME / NURSING-HOME / NURSING-HOME / etc
      CH_WORD = ifelse(
        test = (
          {{ address_col }} %LIKE% "%CARE HOME%" |
          {{ address_col }} %LIKE% "%CARE-HOME%" |
          {{ address_col }} %LIKE% "%NURSING HOME%" |
          {{ address_col }} %LIKE% "%NURSING-HOME%" |
          {{ address_col }} %LIKE% "%RESIDENTIAL HOME%" |
          {{ address_col }} %LIKE% "%RESIDENTIAL-HOME%" |
          {{ address_col }} %LIKE% "%REST HOME%" |
          {{ address_col }} %LIKE% "%REST-HOME%"
        ),
        yes = 1,
        no = 0
      ),
      # If it has this in then it is definitely not a care home
      NON_CH_WORD = ifelse(
        test = (
          {{ address_col }} %LIKE% "%ABOVE%" |
          {{ address_col }} %LIKE% "%CARAVAN%" |
          {{ address_col }} %LIKE% "%CHILDREN%" |
          {{ address_col }} %LIKE% "%CONVENT%" |
          {{ address_col }} %LIKE% "%HOLIDAY%" |
          {{ address_col }} %LIKE% "%MARINA%" |
          {{ address_col }} %LIKE% "%MOBILE%" |
          {{ address_col }} %LIKE% "%MONASTERY%" |
          {{ address_col }} %LIKE% "%NO FIXED ABODE%" |
          {{ address_col }} %LIKE% "%RESORT%" |
          {{ address_col }} %LIKE% "%RECOVERY%"
        ),
        yes = 1,
        no = 0
      )
    )
  
  # Calculate the care home flag
  if (use_addressbase_plus_class_col) {
    # If we have the AddressBase plus class then we can use that
    df <- df %>%
      dplyr::mutate(
        CH_FLAG = ifelse(
          test = 
            (CLASS == "RI01" | CH_WORD == 1) & (NON_CH_WORD == 0),
          yes = 1,
          no = 0
        )
      )
    
  } else {
    # Otherwise just use the single line address field
    df <- df %>%
      dplyr::mutate(
        CH_FLAG = ifelse(
          test = CH_WORD == 1 & NON_CH_WORD == 0, 
          yes = 1, 
          no = 0
        )
      )
    
  }
  
  # Drop the calculated columns
  df %>%
    dplyr::select(-c(CH_WORD, NON_CH_WORD))
    
}
