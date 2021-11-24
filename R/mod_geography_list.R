#' geography_list UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_geography_list_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput("geography",
      label = "List of geographies",
      choices = stp_list,
      multiple = FALSE
    ),
  )
}

#' geography_list Server Functions
#'
#' @noRd
mod_geography_list_server <- function(id) {
  moduleServer(id, function(input, output, session, input_view) {
    ns <- session$ns

    # keep list of STP and LA
    stp_list <- careHomePrescribingScrollytellR::stp_la_lookup %>%
      dplyr::distinct(STP)

    observe({
      if (input_view() == "STP") {
        updateSelectInput(session, "geography", choices = stp_list)
      }
    })
  })
}

## To be copied in the UI
# mod_geography_list_ui("geography_list_ui_1")

## To be copied in the server
# mod_geography_list_server("geography_list_ui_1")
