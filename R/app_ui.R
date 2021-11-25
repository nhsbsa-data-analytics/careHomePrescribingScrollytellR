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
      br(),
      # scrollytell::scrolly_container(
      #   outputId = "scrolly1",
      #   scrollytell::scrolly_graph(),
      #   scrollytell::scrolly_sections(
      #     scrollytell::scrolly_section(
      #       id = "dummy"
      #     ),
      #     br(),
      #     scrollytell::scrolly_section(
      #       id = "02_overall_summary",
      #       mod_02_overall_summary_ui(id = "02_overall_summary_1")
      #     )
      #   )
      # ),
      # mod_02_overall_summary_ui(id = "02_overall_summary_1"),
      scrollytell::scrolly_container(
        outputId = "scrolly",
        scrollytell::scrolly_graph(
          # mod_radio_ui(id = "radio_ui_1")
          # mod_drop_down_ui("drop_down_ui_1")
        ),
        scrollytell::scrolly_sections(
          scrollytell::scrolly_section(
            id = "dummy"
          ),
          br(),
          scrollytell::scrolly_section(
            id = "02_overall_summary",
            mod_02_overall_summary_ui(id = "02_overall_summary_1")
          ),
          br(),
          scrollytell::scrolly_section(
            id = "geography_selection",
            # mod_03_select_geography_ui("03_select_geography_ui_1")
            p(
              "Select a sustainability and transofmration plan (STP) geography, Regionor or local authority for a personalised look at care home profile.",
              "Individual STP, Region or local authority can be selected in each chart."
            ),
            selectInput(
              inputId = "input_view",
              label = h5("Select geography:"),
              choices = c("Overall", "STP", "Region", "Local Authority"),
              selected = "Overall"
            ),
            uiOutput(outputId = "geo_level2")
            # mod_geography_list_ui(id = "geography_list_1")
          ),
          br(),
          scrollytell::scrolly_section(
            id = "04_patients_by_gender_and_age_band",
            mod_04_patients_by_gender_and_age_band_chart_ui(
              id = "04_patients_by_gender_and_age_band_chart_1"
            )
          ),
          br(),
          scrollytell::scrolly_section(
            id = "bnf_carehome_item",
            mod_bnf_ch_item_treemap_ui("bnf_ch_item_treemap_1")
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
