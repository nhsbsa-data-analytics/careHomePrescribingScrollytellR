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


  # moduleServer(
  #   id = "03_items_per_patient_chart_1",
  #   module = mod_03_items_per_patient_chart_server
  # )


  # selected_geography <- callModule(mod_util_data_server,
  #                                  "util_data_ui_1",
  #                                  radio_input = radio_input)
  #
  radio_input <- mod_radio_server("radio_ui_1")
  mod_drop_down_server("drop_down_ui_1")

  mod_03_select_geography_server("03_select_geogrphy_ui_1")

  callModule(
    id = "04_patients_by_gender_and_age_band_chart_1",
    module = mod_04_patients_by_gender_and_age_band_chart_server,
    input_view = radio_input
  )
  moduleServer(
    id = "bnf_ch_item_treemap_1",
    module = mod_bnf_ch_item_treemap_server
  )
  moduleServer(
    id = "99_footer_1",
    module = mod_99_footer_server
  )

  output$scrolly <- scrollytell::renderScrollytell({
    scrollytell::scrollytell()
  })
}
