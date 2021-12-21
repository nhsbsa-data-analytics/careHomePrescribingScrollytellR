#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Your application server logic
  mod_00_header_server("00_header_ui_1")
  mod_01_intro_server("01_intro_ui_1")
  mod_02_demographics_server("02_demographics_ui_1")
  mod_03_overall_summary_server("03_overall_summary_ui_1")
  mod_04_estimated_care_home_patients_server("04_estimated_care_home_patients_ui_1")
  mod_05_items_and_cost_per_bnf_chapter_and_section_chart_server("05_items_and_cost_per_bnf_chapter_and_section_chart_ui_1")
  mod_definitions_server("definitions_ui_1")
  mod_08_footer_server("08_footer_ui_1")
  mod_add_two_plots_server("add_two_plots_ui_1")

  output$scrolly <- scrollytell::renderScrollytell({
    scrollytell::scrollytell()
  })
}
