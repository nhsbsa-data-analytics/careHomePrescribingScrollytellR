#' chart_example UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_chart_example_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(
      inputId = ns("bins"),
      label = "Number of bins:",
      min = 1,
      max = 50,
      value = 30
    ),
    highcharter::highchartOutput(
      outputId = ns("chart")
    )
  )
}

#' chart_example Server Function
#'
#' @noRd
mod_chart_example_server <- function(input, output, session) {
  ns <- session$ns

  output$chart <- highcharter::renderHighchart({

    # Create plot
    careHomePrescribingScrollytellR::items_per_patient_df %>%
      dplyr::arrange(YEAR_MONTH) %>% 
      dplyr::mutate(YEAR_MONTH = lubridate::ym(YEAR_MONTH)) %>% 
      highcharter::hchart(
        type = "line",
        highcharter::hcaes(x = YEAR_MONTH, y = ITEMS_PER_PATIENT, group = CH_FLAG)
      )
  })
  
}

## To be copied in the UI
# mod_chart_example_ui("chart_example_1")

## To be copied in the server
# callModule(mod_chart_example_server, "chart_example_1")
