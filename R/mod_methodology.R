#' methodology UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_methodology_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Methodology"),
    p(
      enurl(
        text = "Here",
        url = "https://rpubs.com/nhsbsa-data-analytics/862168"
      ),
      " is a link to methodology described in full."
    )
  )
}

#' methodology Server Functions
#'
#' @noRd
mod_methodology_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_methodology_ui("methodology_ui_1")

## To be copied in the server
# mod_methodology_server("methodology_ui_1")
