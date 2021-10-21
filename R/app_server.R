#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  moduleServer("00_header_1", mod_00_header_server)
  moduleServer("markdown_example_1", mod_markdown_example_server)
  moduleServer("items_per_patient_chart_1", mod_items_per_patient_chart_server)
  moduleServer("99_footer_1", mod_99_footer_server)
}
