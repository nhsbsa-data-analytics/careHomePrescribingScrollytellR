#' 05_common_drugs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_05_common_drugs_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' 05_common_drugs Server Functions
#'
#' @noRd 
mod_05_common_drugs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_05_common_drugs_ui("05_common_drugs_ui_1")
    
## To be copied in the server
# mod_05_common_drugs_server("05_common_drugs_ui_1")
