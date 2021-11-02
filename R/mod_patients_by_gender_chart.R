#' patients_by_gender_chart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_patients_by_gender_chart_ui <- function(id) {
  ns <- NS(id)
  tagList(
    highcharter::highchartOutput(
      outputId = ns("patients_by_gender_chart")
    )
  )
}

#' patients_by_gender_chart Server Function
#'
#' @noRd
mod_patients_by_gender_chart_server <- function(input, output, session) {
  ns <- session$ns

  output$patients_by_gender_chart <- highcharter::renderHighchart({

    # Create plot
    careHomePrescribingScrollytellR::patients_by_gender_df %>%
      highcharter::hchart(
        type = "column",
        highcharter::hcaes(x = PDS_GENDER, y = PCT * 100, group = CH_FLAG)
      ) %>%
      theme_nhsbsa(palette = "highlight") %>%
      highcharter::hc_title(
        text = "Percentage of patients by Gender (2020/21)"
      ) %>%
      highcharter::hc_yAxis(min = 0, max = 100)
  })
}

## To be copied in the UI
# mod_patients_by_gender_chart_ui("patients_by_gender_chart_1")

## To be copied in the server
# callModule(mod_patients_by_gender_chart_server, "patients_by_gender_chart_1")
