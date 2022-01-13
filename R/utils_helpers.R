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

  # Set the thousands seperator
  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- ","
  options(highcharter.lang = hcoptslang)

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

#' Define the breakdowns
#'
#' Define the labels of the breakdowns (in order of hierarchy) with the columns
#' that are used to aggregate
#'
#' @export
breakdowns <- list(
  "Overall" = c(SUB_BREAKDOWN_NAME = "OVERALL"),
  "Geographical - Region" = c(
    SUB_BREAKDOWN_CODE = "PCD_REGION_CODE",
    SUB_BREAKDOWN_NAME = "PCD_REGION_NAME"
  ),
  "Geographical - STP" = c(
    SUB_BREAKDOWN_CODE = "PCD_STP_CODE",
    SUB_BREAKDOWN_NAME = "PCD_STP_NAME"
  ),
  "Geographical - Local Authority" = c(
    SUB_BREAKDOWN_CODE = "PCD_LAD_CODE",
    SUB_BREAKDOWN_NAME = "PCD_LAD_NAME"
  ),
  "Demographical - Gender" = c(SUB_BREAKDOWN_NAME = "GENDER"),
  "Demographical - Age Band" =  c(SUB_BREAKDOWN_NAME = "AGE_BAND"),
  "Additional - Gender and Age Band" = c(
    GENDER = "GENDER", 
    AGE_BAND = "AGE_BAND"
  ),
  "Additional - Care home type" = c(
    NURSING_HOME_FLAG = "NURSING_HOME_FLAG", 
    RESIDENTIAL_HOME_FLAG = "RESIDENTIAL_HOME_FLAG"
  )
)


#' Define the geographys
#'
#' Extract them from the breakdowns.
#'
#' @export
geographys <- breakdowns %>% 
  purrr::keep(
    .x = (.),
    .p = stringr::str_detect(
      string = names(.), 
      pattern = "Overall|Geographical - "
    )
  ) %>% 
  purrr::set_names(
    x = stringr::str_replace(
      string = names(.), 
      pattern = "Overall|Geographical - ", 
      replacement = ""
    )
  )

#' Format data-raw table
#'
#' Deal with factors and sort table.
#'
#' @param df Dataframe
#' @param vars Grouping variables
#'
#' @return
#' @export
format_data_raw <- function(df, vars) {

  # Initially sort the factors
  df <- df %>%
    dplyr::arrange(
      dplyr::across(
        dplyr::any_of(
          c("YEAR_MONTH", "SUB_BREAKDOWN_NAME", "SUB_GEOGRAPHY_NAME", vars)
        )
      )
    )

  # Move overall to the first category
  df <- df %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::any_of(
          c("YEAR_MONTH", "SUB_BREAKDOWN_NAME", "SUB_GEOGRAPHY_NAME")
        ),
        .fns = ~ forcats::fct_relevel(.x, "Overall")
      )
    )

  # Breakdown is a hierarchy
  if ("BREAKDOWN" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(
        BREAKDOWN = forcats::fct_relevel(BREAKDOWN, names(breakdowns))
      )
  }

  # Geography is a hierachy
  if ("GEOGRAPHY" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(
        GEOGRAPHY = forcats::fct_relevel(GEOGRAPHY, names(geographys))
      )
  }

  # Sort final dataframe by new factors
  df %>%
    dplyr::arrange(
      dplyr::across(
        dplyr::any_of(
          c(
            "YEAR_MONTH",
            "BREAKDOWN",
            "SUB_BREAKDOWN_NAME",
            "GEOGRAPHY",
            "SUB_GEOGRAPHY_NAME",
            vars
          )
        )
      )
    )
}

#' fontawesome save to datauri
#' taken from https://jkunst.com/highcharter/articles/fontawesome.html
#'
#' @param name fontawsome name
#' @param vars Grouping variables
#'
#' @return
#' @export

fa_to_png_to_datauri <- function(name, ...) {
  tmpfl <- tempfile(fileext = ".png")

  fontawesome::fa_png(name, file = tmpfl, ...)

  knitr::image_uri(tmpfl)
}
