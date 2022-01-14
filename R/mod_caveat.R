#' caveat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_caveat_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' caveat Server Functions
#'
#' @noRd
mod_caveat_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_caveat_ui("caveat_ui_1")

## To be copied in the server
# mod_caveat_server("caveat_ui_1")
