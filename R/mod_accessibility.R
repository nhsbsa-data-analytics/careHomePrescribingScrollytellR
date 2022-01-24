#' accessibility UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_accessibility_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Accessibility"),
    br(),
    p("Here is a page containing all of our accessibility statement"),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
  )
}

#' accessibility Server Functions
#'
#' @noRd
mod_accessibility_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_accessibility_ui("accessibility_ui_1")

## To be copied in the server
# mod_accessibility_server("accessibility_ui_1")
