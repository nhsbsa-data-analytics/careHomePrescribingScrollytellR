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
      " in England in 2020/21"
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
      "This is based on experimental data linkage work. We welcome feedback, ",
      "collaboration and refinement of the methodology to see if it can be ",
      "used in NHSBSA information systems in the future and we can develop ",
      "additional analyses."
    ),
    br(),
    p(
      "We estimate that there was a monthly average of",
      tags$b("286 thousand care home patients"), "receiving prescriptions ",
      "each month in 2020/21. They received an estimated", tags$b("35 million"),
      "prescription items", tags$b("at a cost of £324 million"), "during ",
      "2020/21."
    ),
    p(
      "This accounts for around", tags$b("5%"), "of ",
      tippy(
        text = "older patients",
        tooltip = tooltip_text$older
      ),
      "receiving prescription items each month and", tags$b("7%"), "of the ",
      "total primary care drug spend for ",
      tippy(
        text = "older patients",
        tooltip = tooltip_text$older
      ),
      "during 2020/21."
    ),
    p(
      "As might be expected,",
      tags$b("care home patients receive more prescribing"), "than non-care ",
      "home patients receiving prescriptions. They also receive a different ",
      "range of medicines, and are somewhat more likely to receive ",
      "prescribing for pain relief."
    ),
    p(
      "Average monthly prescribing costs and volumes per care home patient ",
      "vary by age, gender, care home type and geography."
    ),
    br(),
    p(
      "Prescribing estimates are based on a sophisticated methodology which ",
      "includes linking primary care prescription address data to care home ",
      "addresses ",
      "in ",
      enurl(
        text = "AddressBase Plus",
        url = "https://www.ordnancesurvey.co.uk/business-government/products/addressbase"
      ),
      "to create a prescribing dataset. The address matching method has been ",
      "available as an ",
      enurl(
        text = "R package.",
        url = "https://github.com/nhsbsa-data-analytics/addressMatchR"
      )
    ),
    p(
      "These estimates address a key gap in knowledge and give valuable ",
      "insights which could inform the use and management of medicines in ",
      "care homes to help improve health outcomes, the quality of care and ",
      "ensure value. It should however be noted that the analysis period is",
      "at the height of the COVID-19 pandemic and patterns may change in ",
      "subsequent years."
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
