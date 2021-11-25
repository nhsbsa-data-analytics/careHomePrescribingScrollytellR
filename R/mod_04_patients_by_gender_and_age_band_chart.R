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
      align = "center",
      style = "background-color: #FFFFFF;", # will rearrange the button location
      # selectInput("geography", label = "List of geographies", choices = stp_list, selected = "Overall", multiple = FALSE),
      highcharter::highchartOutput(
        outputId = ns("patients_by_gender_and_age_band_chart"),
        height = "500px",
        width = "800px"
      )
    ) # ,
    # fluidRow(
    #   # style = "background-color: #FFFFFF;", # will rearrange the button location
    #   # selectInput("geography", label = h4("List of geographies"), choices = NULL, selected = "Overall", multiple = FALSE),
    #   # uiOutput(ns("geo_level2"))
    #   mod_slider_ui("slider_ui_1", width = "60%")
    # )
  )
}

#' patients_by_gender_and_age_band_chart Server Function
#'
#' @noRd
mod_04_patients_by_gender_and_age_band_chart_server <- function(input, output, session, r, geo_selection) {
  ns <- session$ns

  # Pull the slider value
  year_month <- reactive({
    mod_slider_server("slider_1")
  })


  # Pull the level geography value
  observe({
    r$dataset
    print(r$dataset)
  })

  geo_level1 <- reactive(r$dataset)


  observe({
    geo_selection$value
    # print(geo_selection$value)
  })

  geo_level2 <- reactive(geo_selection$value)

  # print(selected_geography)

  # pull the level 2 geography value

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
  # Here, i need to modify code to show
  # It is
  plot_df <- reactive({
    if (length(geo_level2()) == 0 | geo_level2() == "Overall") {
      d <- careHomePrescribingScrollytellR::patients_by_gender_and_age_band_df %>%
        dplyr::filter(!(PDS_GENDER %in% ("Unknown"))) %>%
        dplyr::filter(LEVEL == "Overall" & GEOGRAPHY == "Overall") %>%
        dplyr::group_by(YEAR_MONTH) %>%
        dplyr::mutate(p = TOTAL_PATIENTS / sum(TOTAL_PATIENTS) * 100) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(p = p * ifelse(PDS_GENDER == "Male", 1, -1))
      # print(tail(d))
    } else if (geo_level1() == "Local Authority") {
      careHomePrescribingScrollytellR::patients_by_gender_and_age_band_df %>%
        dplyr::filter(!(PDS_GENDER %in% ("Unknown"))) %>%
        dplyr::filter(LEVEL == "LA" & GEOGRAPHY == geo_level2()) %>%
        dplyr::group_by(YEAR_MONTH) %>%
        dplyr::mutate(p = TOTAL_PATIENTS / sum(TOTAL_PATIENTS) * 100) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(p = p * ifelse(PDS_GENDER == "Male", 1, -1))
      # print(head(d))
    } else {
      careHomePrescribingScrollytellR::patients_by_gender_and_age_band_df %>%
        dplyr::filter(!(PDS_GENDER %in% ("Unknown"))) %>%
        dplyr::filter(LEVEL == geo_level1() & GEOGRAPHY == geo_level2()) %>%
        dplyr::group_by(YEAR_MONTH) %>%
        dplyr::mutate(p = TOTAL_PATIENTS / sum(TOTAL_PATIENTS) * 100) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(p = p * ifelse(PDS_GENDER == "Male", 1, -1))
      # print(head(d))
    }
  })

  # # Pull the max p
  max_p <- reactive({
    max(abs(plot_df()$p))
  })

  # Format for highcharter animation
  plot_series_list <- reactive({
    plot_df() %>%
      tidyr::expand(YEAR_MONTH, AGE_BAND, PDS_GENDER) %>%
      dplyr::left_join(plot_df()) %>%
      dplyr::mutate(p = tidyr::replace_na(p)) %>%
      dplyr::group_by(AGE_BAND, PDS_GENDER) %>%
      dplyr::do(data = list(sequence = .$p)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(PDS_GENDER) %>%
      dplyr::do(data = .$data) %>%
      dplyr::mutate(name = PDS_GENDER) %>%
      highcharter::list_parse()

    # print(f)
  })

  # Pyramid plot for age band and gender
  output$patients_by_gender_and_age_band_chart <- highcharter::renderHighchart({
    highcharter::highchart() %>%
      highcharter::hc_chart(type = "bar", marginBottom = 100) %>%
      highcharter::hc_add_series_list(x = plot_series_list()) %>%
      highcharter::hc_motion(
        labels = unique(plot_df()$YEAR_MONTH),
        series = c(0, 1)
      ) %>%
      theme_nhsbsa(palette = "gender") %>%
      highcharter::hc_title(
        text = "Age band and gender of estimated care home residents in England (2020/21)"
      ) %>%
      highcharter::hc_subtitle(
        text = "Note: This excludes individuals without either an age band or gender."
      ) %>%
      highcharter::hc_xAxis(
        categories = sort(unique(plot_df()$AGE_BAND)),
        reversed = FALSE
      ) %>%
      highcharter::hc_yAxis(
        min = -ceiling(max_p() / 5) * 5,
        max = ceiling(max_p() / 5) * 5,
        labels = list(
          formatter = highcharter::JS("function(){ return Math.abs(this.value) + '%' ;}")
        )
      ) %>%
      highcharter::hc_tooltip(
        shared = FALSE,
        formatter = highcharter::JS("function () { return '<b>Gender: </b>' + this.series.name + '<br>' + '<b>Age band (5 years): </b>' + this.point.category + '<br/>' + '<b>Percentage: </b>' + Math.abs(Math.round(this.point.y * 10) / 10) + '%';}")
      )
  })
}


# https://rpubs.com/jbkunst/stackoverflow-questions-3597922








## To be copied in the UI
# mod_patients_by_gender_and_age_band_chart_ui("patients_by_gender_and_age_band_chart_1")

## To be copied in the server
# callModule(mod_patients_by_gender_and_age_band_chart_server, "patients_by_gender_and_age_band_chart_1")
