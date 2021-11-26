#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  moduleServer(
    id = "00_header_1",
    module = mod_00_header_server
  )
  moduleServer(
    id = "01_intro_1",
    module = mod_01_intro_server
  )
  moduleServer(
    id = "02_overall_summary_1",
    module = mod_02_overall_summary_server
  )

  mod_03_patients_by_gender_and_age_band_chart_server("03_patients_by_gender_and_age_band_chart_1")

  mod_04_common_drugs_server("04_common_drugs_ui_1")

  moduleServer(
    id = "99_footer_1",
    module = mod_99_footer_server
  )

  output$scrolly <- scrollytell::renderScrollytell({
    scrollytell::scrollytell()
  })
}
