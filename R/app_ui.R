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
    mod_00_header_ui("00_header_ui_1"),
    br(),
    navlistPanel(
      well = FALSE,
      widths = c(2, 12), # Z index set on the 2 so it is above the 12
      tabPanel(
        title = "Article",
        fluidPage(
          br(),
          mod_01_intro_ui("01_intro_ui_1"),
          scrollytell::scrolly_container(
            outputId = "scrolly",
            scrollytell::scrolly_graph(),
            scrollytell::scrolly_sections(
              scrollytell::scrolly_section(
                id = "dummy"
              ),
              br(),
              scrollytell::scrolly_section(
                id = "02_demographics",
                mod_02_demographics_ui("02_demographics_ui_1")
              ),
              br(),
              scrollytell::scrolly_section(
                id = "03_care_home_prescribing",
                mod_03_care_home_prescribing_ui("03_care_home_prescribing_ui_1")
              ),
              br(),
              scrollytell::scrolly_section(
                id = "05_items_and_cost_per_bnf",
                mod_05_items_and_cost_per_bnf_ui("05_items_and_cost_per_bnf_ui_1")
              ),
              br()
            )
          )
        )
      ),
      tabPanel(
        title = "Definitions",
        fluidPage(
          br(),
          mod_definitions_ui("definitions_ui_1")
        )
      ),
      tabPanel(
        title = "Caveats",
        fluidPage(
          br(),
          mod_caveat_ui("caveat_ui_1")
        )
      ),
      tabPanel(
        title = "Methodology",
        fluidPage(
          br(),
          mod_methodology_ui("methodology_ui_1")
        )
      ),
      tabPanel(
        title = "Accessibility",
        fluidPage(
          br(),
          mod_accessibility_ui("accessibility_ui_1")
        )
      ),
    ),
    br(),
    mod_08_footer_ui("08_footer_ui_1")
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
