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
    mod_00_header_ui("00_header_1"),
    br(),
    fluidPage(
      mod_01_intro_ui("01_intro_1"),
      scrollytell::scrolly_container(
        outputId = "scrolly",
        scrollytell::scrolly_graph(),
        scrollytell::scrolly_sections(
          scrollytell::scrolly_section(
            id = "dummy"
          ),
          br(),
          scrollytell::scrolly_section(
            id = "02_overall_summary",
            mod_02_overall_summary_ui("02_overall_summary_1")
          ),
          br(),
          scrollytell::scrolly_section(
            id = "items_per_patient_chart",
            mod_items_per_patient_chart_ui("items_per_patient_chart_1")
          ),
          br(),
          scrollytell::scrolly_section(
            id = "patients_by_gender_and_age_band_chart",
            mod_patients_by_gender_and_age_band_chart_ui("patients_by_gender_and_age_band_chart_1")
          ),
          br(),
          scrollytell::scrolly_section(
            id = "items_and_cost_per_bnf_chapter_and_section_chart",
            mod_items_and_cost_per_bnf_chapter_and_section_chart_ui("items_and_cost_per_bnf_chapter_and_section_chart_1")
          )
        )
      )
    ),
    br(),
    mod_99_footer_ui("99_footer_1")
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
      app_title = "careHomePrescribingScrollytellR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
