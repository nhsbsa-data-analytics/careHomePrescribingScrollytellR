#' radio UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_radio_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyWidgets::prettyRadioButtons(
      inputId = ns("input_view"),
      label = h5("Select geography:"),
      choices = c("STP", "Local Authority"),
      shape = "curve",
      selected = "STP",
      inline = TRUE
    )
  )
}

#' radio Server Functions
#'
#' @noRd
mod_radio_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    return(
      reactive({
        input$input_view
      })
    )
  })
}

## To be copied in the UI
# mod_radio_ui("radio_ui_1")

## To be copied in the server
# mod_radio_server("radio_ui_1")
