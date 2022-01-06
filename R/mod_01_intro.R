#' 01_intro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_01_intro_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4(
      "Estimated primary care prescribing patterns for ",
      tippy(
        text = "older care home patients",
        tooltip = tooltip_text$care_home
      ),
      " in England"
    ),
    p(
      "In this article we provide unique insight into primary care ",
      "prescribing patterns for ",
      tippy(
        text = "older care home patients",
        tooltip = tooltip_text$care_home
      ),
      " in England during 2020/21."
    ),
    p(
      "We estimate that there was a monthly average of", tags$b("285 thousand"),
      " care home patients receiving prescriptions each month in 2020/21. ",
      "They received an estimated 35 million prescription items at a cost of ",
      tags$b("£320 million"), " during 2020/21."
    ),
    p(
      "This accounts for around 4% of ",
      tippy(
        text = "older patients",
        tooltip = tooltip_text$older
      ),
      "receiving prescription items each month and 7% of the total primary ",
      "care drug spend for ",
      tippy(
        text = "older patients",
        tooltip = tooltip_text$older
      ),
      "during 2020/21."
    ),
    p(
      "Prescribing estimates are based on a sophisticated methodology which ",
      "matches primary care prescription address data to care home addresses ",
      "in ",
      enurl(
        text = "AddressBase Plus",
        url = "https://www.ordnancesurvey.co.uk/business-government/products/addressbase"
      ),
      "to create a prescribing dataset. The matching method has been ",
      "available as an ",
      enurl(
        text = "R package",
        url = "https://github.com/nhsbsa-data-analytics/addressMatchR"
      ),
      "."
    ),
    p(
      "These estimates address a key gap in knowledge and give valuable ",
      "insights which could inform the use and management of medicines in ",
      "care homes to help improve health outcomes, the quality of care and ",
      "ensure value."
    ),
    p(
      "This is an experimental piece of work and we are interested in ",
      "collaboration and refining the methodology so that it can be used in ",
      "NHS information systems in future."
    )
  )
}

#' 01_intro Server Functions
#'
#' @noRd
mod_01_intro_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_01_intro_ui("01_intro_ui_1")

## To be copied in the server
# mod_01_intro_server("01_intro_ui_1")
