#' util_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_util_data_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' util_data Server Functions
#'
#' @noRd
mod_util_data_server <- function(id) {
  moduleServer(id, function(input, output, session, radio_input = radio_input) {
    ns <- session$ns

    stp_list <- reactive({
      careHomePrescribingScrollytellR::stp_la_lookup %>%
        distinct(radio_input$input_view)
    })

    vals <- reactiveVals()

    observe({
      vals$stp_list <- stp_list()
    })

    return(vals)
  })
}

## To be copied in the UI
# mod_util_data_ui("util_data_ui_1")

## To be copied in the server
# mod_util_data_server("util_data_ui_1")
