#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    mod_00_header_ui(id = "00_header_1"),
    br(),
    fluidPage(
      mod_01_intro_ui(id = "01_intro_1"),
      mod_items_per_patient_chart_ui(id = "items_per_patient_chart_1"),
      mod_patients_by_gender_and_age_band_chart_ui(
        id = "patients_by_gender_and_age_band_chart_1"
      ),
      mod_overall_summary_ui(id = "overall_summary_1")
    ),
    br(),
    mod_99_footer_ui(id = "99_footer_1")
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "nhsbsaShinyR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
