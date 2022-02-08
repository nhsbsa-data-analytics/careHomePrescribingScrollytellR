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
      "for ",
      tippy(
        text = "older care home patients",
        tooltip = tooltip_text$care_home
      ),
      " in England during 2020/21."
    ),
    p(
      "It is based on experimental data linkage work. We believe this could ",
      "have important utility in terms of informing decisions and improving ",
      "outcomes in future. We welcome your views and input on the ",
      "methodology, if and how the insight can be used and how it can be ",
      "developed further."
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
