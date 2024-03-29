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
    # Need this for accessibility
    tags$html(lang = "en"),
    # Need this for shiny bootstrap dependencies
    bootstrapLib(),
    # First level UI elements
    nhs_header(),
    br(),
    tags$div(
      class = "nhsuk-width-container",
      tags$div(
        class = "nhsuk-main-wrapper",
        id = "maincontent",
        role = "main",
        nhs_navlistPanel(
          well = FALSE,
          widths = c(2, 10),
          tabPanel(
            title = "Article",
            mod_01_intro_ui("01_intro_ui_1"),
            scrollytell::scrolly_container(
              outputId = "scrolly",
              scrollytell::scrolly_graph(),
              scrollytell::scrolly_sections(
                scrollytell::scrolly_section(
                  id = "02_demographics",
                  mod_02_demographics_ui("02_demographics_ui_1")
                ),
                scrollytell::scrolly_section(
                  id = "03_care_home_prescribing",
                  mod_03_care_home_prescribing_ui("03_care_home_prescribing_ui_1")
                ),
                scrollytell::scrolly_section(
                  id = "04_commonly_prescribed_medicines",
                  mod_04_commonly_prescribed_medicines_ui("04_commonly_prescribed_medicines_ui_1")
                ),
                scrollytell::scrolly_section(
                  id = "05_final_thoughts",
                  mod_05_final_thoughts_ui("05_final_thoughts_ui_1")
                )
              )
            )
          ),
          tabPanel(
            title = "Definitions",
            mod_definitions_ui("definitions_ui_1")
          ),
          tabPanel(
            title = "Methodology",
            mod_methodology_ui("methodology_ui_1")
          ),
          tabPanel(
            title = "Caveats",
            mod_caveats_ui("caveats_ui_1")
          )
        ),
        tags$script(" $(document).ready(function () {
      $('#maincontent a[data-toggle=\"tab\"]').on('click', function (e) {
         window.scrollTo(0, 0)
            });
            });"),
        tags$head(tags$script(src = "survey.js"))
      )
    ),
    br(),
    nhs_footer()
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
      app_title = "Estimated prescribing patterns for care home patients aged 65 years or over"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
