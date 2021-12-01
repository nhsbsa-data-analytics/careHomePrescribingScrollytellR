#' patients_by_geography_and_gender_and_age_band_chart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_patients_by_geography_and_gender_and_age_band_chart_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Demographic estimates for older care home patients receiving prescriptions"),
    p(
      tags$b("Two thirds"), " are ", tags$b("female"), "(66%) and", tags$b("one quarter"), "are", tags$b("female aged 90+ years"), ". This is",
      "reasonably consistent across local authorities and STPs."
    ),
    p(
      "The age and gender profile is broadly comparable to",
      a(
        "ONS Estimates of care home patients from April 2020.",
        href = "https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/12215carehomeandnoncarehomepopulationsusedinthedeathsinvolvingcovid19inthecaresectorarticleenglandandwales",
        target = "_blank"
      ),
    ),
    br(),
    br(),
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;",
      h6(
        "Age band and gender of estimated older care home patients in England ",
        "(2020/21)"
      ),
      col_6(
        selectInput(
          inputId = ns("level"),
          label = "Geography",
          choices = c("Overall", "Region", "STP", "Local Authority"),
          width = "100%"
        )
      ),
      col_6(
        selectInput(
          inputId = ns("geography"),
          label = "Sub-geography",
          choices = NULL, # dynamically generated
          width = "100%"
        )
      ),
      highcharter::highchartOutput(
        outputId = ns("patients_by_geography_and_gender_and_age_band_chart"),
        height = "500px",
        width = "800px"
      )
    )
  )
}

#' patients_by_geography_and_gender_and_age_band_chart Server Function
#'
#' @noRd
mod_patients_by_geography_and_gender_and_age_band_chart_server <- function(input,
                                                                           output,
                                                                           session) {
  ns <- session$ns

  # Filter to relevant data for this chart
  patients_by_geography_and_gender_and_age_band_df <-
    careHomePrescribingScrollytellR::patients_by_geography_and_gender_and_age_band_df %>%
    dplyr::filter(dplyr::across(c(LEVEL, GEOGRAPHY, GENDER), not_na))

  # Handy resource: https://mastering-shiny.org/action-dynamic.html

  # Filter the data based on the level
  level_df <- reactive({
    req(input$level)
    patients_by_geography_and_gender_and_age_band_df %>%
      dplyr::filter(LEVEL == input$level)
  })

  # Update the list of choices for geography from the rows in level dataframe
  observeEvent(
    eventExpr = level_df(),
    handlerExpr = {
      updateSelectInput(
        inputId = "geography",
        choices = unique(level_df()$GEOGRAPHY)
      )
    }
  )

  # Filter the data based on the level and format for the plot
  plot_df <- reactive({
    req(input$geography)
    level_df() %>%
      dplyr::filter(GEOGRAPHY == input$geography) %>%
      dplyr::group_by(YEAR_MONTH) %>%
      dplyr::mutate(p = TOTAL_PATIENTS / sum(TOTAL_PATIENTS) * 100) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(p = p * ifelse(GENDER == "Male", 1, -1))
  })

  # Pull the max p
  max_p <- reactive({
    req(input$geography)
    max(abs(plot_df()$p))
  })

  # Format for highcharter animation
  plot_series_list <- reactive({
    req(input$geography)
    plot_df() %>%
      tidyr::complete(YEAR_MONTH, AGE_BAND, GENDER,
        fill = list(value = 0)
      ) %>%
      dplyr::group_by(AGE_BAND, GENDER) %>%
      dplyr::do(data = list(sequence = .$p)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(GENDER) %>%
      dplyr::do(data = .$data) %>%
      dplyr::mutate(name = GENDER) %>%
      highcharter::list_parse()
  })

  # Pyramid plot for age band and gender
  output$patients_by_geography_and_gender_and_age_band_chart <- highcharter::renderHighchart({
    req(input$geography)

    highcharter::highchart() %>%
      highcharter::hc_chart(type = "bar", marginBottom = 100) %>%
      highcharter::hc_add_series_list(x = plot_series_list()) %>%
      highcharter::hc_motion(
        labels = unique(plot_df()$YEAR_MONTH),
        series = c(0, 1)
      ) %>%
      theme_nhsbsa(palette = "gender") %>%
      highcharter::hc_xAxis(
        title = list(text = "Age group"),
        categories = sort(unique(plot_df()$AGE_BAND)),
        reversed = FALSE
      ) %>%
      highcharter::hc_yAxis(
        title = list(text = "Percentage of older care home patients (%)"),
        min = -ceiling(max_p() / 5) * 5,
        max = ceiling(max_p() / 5) * 5,
        labels = list(
          formatter = highcharter::JS("function(){ return Math.abs(this.value);}")
        )
      ) %>%
      highcharter::hc_tooltip(
        shared = FALSE,
        formatter = highcharter::JS("function () { return '<b>Gender: </b>' + this.series.name + '<br>' + '<b>Age band (5 years): </b>' + this.point.category + '<br/>' + '<b>Percentage: </b>' + Math.abs(Math.round(this.point.y * 10) / 10) + '%';}")
      )
  })
}

## To be copied in the UI
# mod_patients_by_geography_and_gender_and_age_band_chart_ui("patients_by_geography_and_gender_and_age_band_chart_1")

## To be copied in the server
# callModule(mod_patients_by_geography_and_gender_and_age_band_chart_server, "patients_by_geography_and_gender_and_age_band_chart_1")
