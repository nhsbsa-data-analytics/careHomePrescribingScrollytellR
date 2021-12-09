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
      "Primary care prescribing patterns for ",
      tippy(
        text = "older care home patients",
        tooltip = tooltip_text$care_home
      ),
      " in England"
    ),
    br(),
    p(
      "In this article we provide unique insight into primary care ",
      "prescribing patterns for ",
      tippy(
        text = "older care home patients",
        tooltip = tooltip_text$care_home
      ),
      " in England during 2020/21."
    ),
    br(),
    p(
      "We estimate that there was a monthly average of", tags$b("284 thousand"),
      "patients recieving prescriptions each month in 2020/21 and they ",
      "received an estimated 34 million prescription items at a cost of",
      tags$b("£318 million.")
    ),
    br(),
    p(
      "This accounts for around", tags$b("X%"), "of ",
      tippy(
        text = "older patients",
        tooltip = tooltip_text$older
      ),
      "receiving prescription items and", tags$b("X%"), "of the total primary ",
      "care drug spend for ",
      tippy(
        text = "older patients",
        tooltip = tooltip_text$older
      ),
      "during 2020/21."
    ),
    br(),
    p(
      "Prescribing estimates are based on a sophisticated methodology which ",
      "matches primary care prescription address data to care home addresses ",
      "in ",
      enurl(
        url = "https://www.ordnancesurvey.co.uk/business-government/products/addressbase",
        text = "AddressBase Plus"
      ),
      "to create a prescribing dataset. The matching method has been ",
      "available as an ",
      enurl(
        url = "https://github.com/nhsbsa-data-analytics/addressMatchR",
        text = "R package"
      ),
      "."
    ),
    p(
      "These estimates address a key gap in knowledge and give valuable ",
      "insights to inform the use and management of medicines in care homes ",
      "to help improve health outcomes, the quality of care and ensure value."
    )
  )
}

#' 01_intro Server Function
#'
#' @noRd
mod_01_intro_server <- function(input, output, session) {
  ns <- session$ns
}

## To be copied in the UI
# mod_01_intro_ui("01_intro_1")

## To be copied in the server
# callModule(mod_01_intro, "01_intro_1")
