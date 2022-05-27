#' 05_final_thoughts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_05_final_thoughts_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2(
      "Final thoughts"
    ),
    p(
      "This article provides estimates of primary care prescribing patterns ",
      "for care home patients aged 65 years or over ",
      "in England during 2020/21, based on experimental data linkage work."
    ),
    p(
      "These estimates address a key gap in knowledge and give valuable insights ",
      "which could inform the use and management of medicines in care homes to help ",
      "improve health outcomes, the quality of care and ensure value."
    ),
    p(
      "We welcome your views and input on the ",
      enurl(
        text = "methodology ",
        url = "https://rpubs.com/nhsbsa-data-analytics/methodology"
      ),
      "and ",
      enurl(
        text = "code,",
        url = "https://github.com/nhsbsa-data-analytics/careHomePrescribingScrollytellR"
      ),
      " if and how the insight can be used and how our work can be developed ",
      "further. You can contact us using the link at the end of this article."
    ),
    br(),
    p(
      "Please take a moment to fill out the ",
      enurl(
        text = "survey",
        url = "https://online1.snapsurveys.com/bsareport?rpt=2"
      ),
      "and your input will be value to our report."
    )
  )
}

#' 05_final_thoughts Server Functions
#'
#' @noRd
mod_05_final_thoughts_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_05_final_thoughts_ui("05_final_thoughts_ui_1")

## To be copied in the server
# mod_05_final_thoughts_server("05_final_thoughts_ui_1")
