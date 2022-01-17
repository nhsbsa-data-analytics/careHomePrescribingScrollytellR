#' 06_common_medicines UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_06_common_medicines_ui <- function(id) {
  ns <- NS(id)
  tagList(
    #h6("Commonly prescribed medicines"),
    p("The top BNF chapters for older care home prescribing in 2020/21 are: "),
    p("By number of prescription items: "),
    tags$ul(
      tags$li(
        style = "font-size: 16pt;",
        "04-Central Nervous System"
      ),
      tags$li(
        style = "font-size: 16pt;",
        "02-Cardiovascular System"
      ),
      tags$li(
        style = "font-size: 16pt;",
        "01-Gastro-intestinal System"
      )
    ),
  p("And by drug cost:"),
  tags$ul(
    tags$li(
      style = "font-size: 16pt;",
      "04-Central Nervous System"
    ),
    tags$li(
      style = "font-size: 16pt;",
      "09-Nutrition and Blood"
    ),
    tags$li(
      style = "font-size: 16pt;",
      "02-Cardiovascular System"
    )
  ),
  p(
  "Around one in four prescription items (24%) prescribed to care home patients ",
  "in 2020/21 are from the", tags$b("central nervous system"), "BNF chapter. ",
  "This chapter also accounts for 24% of drug cost.", tags$b("Analgesics"), "is ",
  "the most common BNF Section within the Central Nervous System for care home ",
  "patients in terms of items and drug cost. "
  ),
  p(
    "And ", tags$b("Paracetamol"), " (painkiller) is the most commonly prescribed ",
    "drug at chemical substance level by number of prescription items, accounting ",
    "for 5% of all prescription items to older care home patients. ", 
    "Whereas it is ", tags$b("Atorvastatin"), "(used to lower cholesterol) in ",
    "older non care home patients."
  ),
  p(
    tags$b("Enteral nutrition"), " products however account for the greatest ",
    "percentage of drug cost at chemical substance level (13%) in older care home ",
    "patients. Whereas it is ", tags$b("Apixiban"), "(blood thinner) in older ", 
    "non care home patients (7% of drug cost), the second most common medicine by ",
    "cost for care home patients."
  ),  
  )
}

#' 06_common_medicines Server Functions
#'
#' @noRd
mod_06_common_medicines_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_06_common_medicines_ui("06_common_medicines_ui_1")

## To be copied in the server
# mod_06_common_medicines_server("06_common_medicines_ui_1")
