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
    id = "02_patients_by_geography_and_gender_and_age_band_chart_1",
    module = mod_02_patients_by_geography_and_gender_and_age_band_chart_server
  )
  moduleServer(
    id = "03_overall_summary_1",
    module = mod_03_overall_summary_server
  )
  moduleServer(
    id = "04_items_and_cost_per_bnf_chapter_and_section_chart_1",
    module = mod_04_items_and_cost_per_bnf_chapter_and_section_chart_server
  )
  moduleServer(
    id = "map_1",
    module = mod_map_server
  )
  moduleServer(
    id = "99_footer_1",
    module = mod_99_footer_server
  )

  output$scrolly <- scrollytell::renderScrollytell({
    scrollytell::scrollytell()
  })
}
