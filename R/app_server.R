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
  mod_03_care_home_prescribing_server("03_care_home_prescribing_ui_1")
  mod_04_commonly_prescribed_medicines_server("04_commonly_prescribed_medicines_ui_1")
  mod_05_final_thoughts_server("05_final_thoughts_ui_1")
  mod_06_footer_server("06_footer_ui_1")
  # mod_definitions_server("definitions_ui_1")
  mod_caveat_server("caveat_ui_1")
  # mod_accessibility_server("accessibility_ui_1")
  # mod_contact_us_server("contact_us_ui_1")

  output$scrolly <- scrollytell::renderScrollytell({
    scrollytell::scrollytell()
  })
}
