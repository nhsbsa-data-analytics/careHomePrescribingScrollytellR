#' 04_patients_by_gender_and_age_band_chart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_04_patients_by_gender_and_age_band_chart_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Demographic estimates for older care home residents receiving prescriptions"),
    p(
      tags$b("Two thirds"), " of older care home residents are ", tags$b("female"), "(66%) compared to around",
      "half (53%) in those aged 65+ years receiving prescriptions in general."
    ),
    p(
      tags$b("One quarter"), " of care home residents in 2020/21 are", tags$b("female aged 90."),
      "The age and gender profile is broadly comparable to",
      a(
        "ONS Estimates of care home residents from April 2020.",
        href = "https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/12215carehomeandnoncarehomepopulationsusedinthedeathsinvolvingcovid19inthecaresectorarticleenglandandwales",
        target = "_blank"
      ),
    ),
    br(),
    fluidRow(
      # style = "background-color: #FFFFFF;", # will rearrange the button location
      # selectInput("geography", label = h4("List of geographies"), choices = NULL, selected = "Overall", multiple = FALSE),
      # uiOutput(ns("geo_level2"))
    ),
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;", # will rearrange the button location
      # selectInput("geography", label = "List of geographies", choices = stp_list, selected = "Overall", multiple = FALSE),
      highcharter::highchartOutput(
        outputId = ns("patients_by_gender_and_age_band_chart"),
        height = "500px",
        width = "800px"
      )
    )
  )
}

#' patients_by_gender_and_age_band_chart Server Function
#'
#' @noRd
mod_04_patients_by_gender_and_age_band_chart_server <- function(input, output, session, input_view) {
  ns <- session$ns




  # output$geo_level2 <- renderUI({
  #   if (input_view() == "STP") {
  #     selectInput("geo",
  #       "Choose sub geography",
  #       choices = stp_list, selected = "Overall"
  #     )
  #   } else if (input_view() == "Local Authority") {
  #     selectInput("geo",
  #       "Choose sub geography",
  #       choices = la_list, selected = "Overall"
  #     )
  #   }
  # })




  # Filter out Co-applicants and Unknowns, calculate %s
  plot_df <-
    careHomePrescribingScrollytellR::patients_by_gender_and_age_band_df %>%
    dplyr::filter(!(PDS_GENDER %in% ("Unknown"))) %>%
    dplyr::filter(LEVEL == "Overall") %>%
    dplyr::group_by(YEAR_MONTH) %>%
    dplyr::mutate(p = TOTAL_PATIENTS / sum(TOTAL_PATIENTS) * 100) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(p = p * ifelse(PDS_GENDER == "Male", 1, -1))

  # Pull the max p
  max_p <- max(abs(plot_df$p))

  # Format for highcharter animation
  plot_series_list <- plot_df %>%
    tidyr::complete(YEAR_MONTH, AGE_BAND, PDS_GENDER,
      fill = list(value = 0)
    ) %>%
    dplyr::group_by(AGE_BAND, PDS_GENDER) %>%
    dplyr::do(data = list(sequence = .$p, number = .$TOTAL_PATIENTS)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(PDS_GENDER) %>%
    dplyr::do(data = .$data) %>%
    dplyr::mutate(name = PDS_GENDER) %>%
    highcharter::list_parse()



  output$patients_by_gender_and_age_band_chart <- highcharter::renderHighchart({
    highcharter::highchart() %>%
      highcharter::hc_chart(type = "bar", marginBottom = 100) %>%
      highcharter::hc_add_series_list(x = plot_series_list) %>%
      highcharter::hc_motion(
        labels = unique(plot_df$YEAR_MONTH),
        series = c(0, 1)
      ) %>%
      theme_nhsbsa(palette = "gender") %>%
      highcharter::hc_title(
        text = "Age group and gender of estimated care home residents receiving prescriptions in England 2020/21"
      ) %>%
      highcharter::hc_subtitle(
        text = "Note: This excludes individuals without either an age band or gender."
      ) %>%
      highcharter::hc_xAxis(
        title = list(text = "Age group"),
        categories = sort(unique(plot_df$AGE_BAND)),
        reversed = FALSE
      ) %>%
      highcharter::hc_yAxis(
        title = list(text = "Number of care home residents as percentage of all care home residents (%)"),
        min = -ceiling(max_p / 5) * 5,
        max = ceiling(max_p / 5) * 5,
        labels = list(
          formatter = highcharter::JS("function(){ return Math.abs(this.value) ;}")
        )
      ) %>%
      highcharter::hc_tooltip(
        shared = FALSE,
        formatter = highcharter::JS(
          "function () { return '<b>Gender: </b>' + this.series.name + '<br>' + '<b>Age band (5 years): </b>' + this.point.category + '<br/>' + '<b>Percentage: </b>' + Math.abs(Math.round(this.point.y * 10) / 10) + '%' + '<br/>' + '<b> Number: </b>' + this.point.number;}"
        )
      )
  })
}


# https://rpubs.com/jbkunst/stackoverflow-questions-3597922








## To be copied in the UI
# mod_patients_by_gender_and_age_band_chart_ui("patients_by_gender_and_age_band_chart_1")

## To be copied in the server
# callModule(mod_patients_by_gender_and_age_band_chart_server, "patients_by_gender_and_age_band_chart_1")
