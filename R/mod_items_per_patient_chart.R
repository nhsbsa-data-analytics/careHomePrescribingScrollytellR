#' items_per_patient_chart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_items_per_patient_chart_ui <- function(id) {
  ns <- NS(id)
  tagList(
    highcharter::highchartOutput(
      outputId = ns("items_per_patient_chart")
    )
  )
}

#' items_per_patient_chart Server Function
#'
#' @noRd
mod_items_per_patient_chart_server <- function(input, output, session) {
  ns <- session$ns

  output$items_per_patient_chart <- highcharter::renderHighchart({

    # Create plot
    careHomePrescribingScrollytellR::items_per_patient_df %>%
      highcharter::hchart(
        type = "line",
        highcharter::hcaes(
          x = YEAR_MONTH, 
          y = ITEMS_PER_PATIENT, 
          group = CH_FLAG
        )
      )
  })
}

## To be copied in the UI
# mod_items_per_patient_chart_ui("items_per_patient_chart_1")

## To be copied in the server
# callModule(mod_items_per_patient_chart_server, "items_per_patient_chart_1")
