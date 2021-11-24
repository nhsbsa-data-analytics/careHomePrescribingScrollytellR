#' 03_select_geography UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_03_select_geography_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 6,
          p(
            "Select a sustainability and transofmration plan (STP) geography, Regionor or local authority for a personalised look at care home profile.",
            "Individual STP, Region or local authority can be selected in each chart."
          )
        ),
        column(
          width = 6,
          mod_radio_ui(id = "radio_ui_1"),
          mod_geography_list_ui(id = "geography_list_1")
        )
      )
    )
  )
}

#' 03_select_geography Server Functions
#'
#' @noRd
mod_03_select_geography_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_03_select_geography_ui("03_select_geography_ui_1")

## To be copied in the server
# mod_03_select_geography_server("03_select_geography_ui_1")
