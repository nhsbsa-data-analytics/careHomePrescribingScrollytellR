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
    br(),
    h6("Older patients"),
    p(
      tags$b("Older patients"),
      "are patients aged 65+ at the time of prescribing."
    ),
    br(),
    h6("Older care home patients"),
    p(
      tags$b("Older care home patients"),
      "are patients aged 65+ who received their prescription whilst ",
      "living in a care home at the time of prescribing."
    ),
    br(),
    h6("Older care home patients"),
    p(
      tags$b("Older care home patients"),
      "are patients aged 65+ who received their prescription whilst ",
      "living in a care home at the time of prescribing."
    ),
    br()
  )
}

#' definitions Server Functions
#'
#' @noRd
mod_definitions_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_definitions_ui("definitions_ui_1")

## To be copied in the server
# mod_definitions_server("definitions_ui_1")
