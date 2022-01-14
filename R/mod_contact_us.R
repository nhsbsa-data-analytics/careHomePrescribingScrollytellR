#' contact_us UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_contact_us_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Contact us"),
    br(),
    p("contact us also add surey link?"),
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

#' contact_us Server Functions
#'
#' @noRd
mod_contact_us_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_contact_us_ui("contact_us_ui_1")

## To be copied in the server
# mod_contact_us_server("contact_us_ui_1")
