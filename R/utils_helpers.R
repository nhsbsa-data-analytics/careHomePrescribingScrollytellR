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


#' Format data-raw table
#'
#' Deal with factors and sort table.
#'
#' @param df 
#' @param ... 
#'
#' @return
#' @export
format_data_raw <- function(df, ...) {

  df %>%
    dplyr::arrange(YEAR_MONTH, SUB_GEOGRAPHY_NAME, ...) %>%
    # Tweak the factors
    dplyr::mutate(
      # Move overall to first category
      dplyr::across(
        .cols = c(YEAR_MONTH, SUB_GEOGRAPHY_NAME),
        .fns = ~ forcats::fct_relevel(.x, "Overall")
      ),
      # Factor is a heirachy
      GEOGRAPHY = forcats::fct_relevel(GEOGRAPHY, "Overall", "Region", "STP")
    ) %>%
    # Sort final dataframe by new factors
    dplyr::arrange(YEAR_MONTH, GEOGRAPHY, SUB_GEOGRAPHY_NAME, ...)
  
}
