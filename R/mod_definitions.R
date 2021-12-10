#' definitions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_definitions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Definitions"),
    br(),
    p("Here is a page containing all of our definitions"),
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

#' definitions Server Function
#'
#' @noRd
mod_definitions_server <- function(input, output, session) {
  ns <- session$ns
}

## To be copied in the UI
# mod_definitions_ui("definitions_1")

## To be copied in the server
# callModule(mod_definitions, "definitions_1")