#' 03_patients_by_gender_and_age_band_chart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_03_patients_by_gender_and_age_band_chart_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Demographic estimates for older care home residents receiving prescriptions"),
    p(
      tags$b("Two thirds"), " of older care home residents are ", tags$b("female"), "(66%) compared to around",
      "half (53%) in those aged 65+ years receiving prescriptions in general."
    ),
    p(
      tags$b("One quarter"), " of care home residents in 2020/21 are", tags$b("female aged 90.")
    ),
    br(),
    fluidRow(
      align = "center",
      radioButtons(
        inputId = ns("input_view"),
        "View Type:",
        c("Count", "Percent"),
        selected = "Count",
        inline = TRUE
      )
    ),
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;", # will rearrange the button location
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
mod_03_patients_by_gender_and_age_band_chart_server <- function(input, output, session) {
  ns <- session$ns

  # Pull the drop down value
  view_sel <- reactive({
    input$input_view
  })

  # observe({
  #   print(view_sel())
  # })


  # Filter out Co-applicants and Unknowns, calculate %s
  plot_df <-
    careHomePrescribingScrollytellR::patients_by_gender_and_age_band_df %>%
    dplyr::filter(!(PDS_GENDER %in% ("Unknown"))) %>%
    dplyr::group_by(YEAR_MONTH) %>%
    dplyr::mutate(p = TOTAL_PATIENTS / sum(TOTAL_PATIENTS) * 100) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(p = p * ifelse(PDS_GENDER == "Male", 1, -1))

  # Pull the max p
  max_p <- max(abs(plot_df$p))

  # Format for highcharter animation
  plot_series_list <- plot_df %>%
    tidyr::expand(YEAR_MONTH, AGE_BAND, PDS_GENDER) %>%
    dplyr::left_join(plot_df) %>%
    dplyr::mutate(p = tidyr::replace_na(p)) %>%
    dplyr::group_by(AGE_BAND, PDS_GENDER) %>%
    dplyr::do(data = list(sequence = .$p)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(PDS_GENDER) %>%
    dplyr::do(data = .$data) %>%
    dplyr::mutate(name = PDS_GENDER) %>%
    highcharter::list_parse()

  # Filter out Co-applicants and Unknowns, calculate %s
  plot_df_count <-
    careHomePrescribingScrollytellR::patients_by_gender_and_age_band_df %>%
    dplyr::filter(!(PDS_GENDER %in% ("Unknown"))) %>%
    dplyr::mutate(TOTAL_PATIENTS = TOTAL_PATIENTS * ifelse(PDS_GENDER == "Male", 1, -1))

  # Pull the max total patients
  max_total_pat <- max(abs(plot_df_count$TOTAL_PATIENTS))

  # Format for highcharter animation
  plot_series_list_count <- plot_df_count %>%
    tidyr::expand(YEAR_MONTH, AGE_BAND, PDS_GENDER) %>%
    dplyr::left_join(plot_df_count) %>%
    dplyr::mutate(TOTAL_PATIENTS = tidyr::replace_na(TOTAL_PATIENTS)) %>%
    dplyr::group_by(AGE_BAND, PDS_GENDER) %>%
    dplyr::do(data = list(sequence = .$TOTAL_PATIENTS)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(PDS_GENDER) %>%
    dplyr::do(data = .$data) %>%
    dplyr::mutate(name = PDS_GENDER) %>%
    highcharter::list_parse()



  output$patients_by_gender_and_age_band_chart <- highcharter::renderHighchart({
    if (view_sel() == "Percent") {
      highcharter::highchart() %>%
        highcharter::hc_chart(type = "bar", marginBottom = 100) %>%
        highcharter::hc_add_series_list(x = plot_series_list) %>%
        highcharter::hc_motion(
          labels = unique(plot_df$YEAR_MONTH),
          series = c(0, 1)
        ) %>%
        theme_nhsbsa(palette = "gender") %>%
        highcharter::hc_title(
          text = "Age band and gender of estimated care home residents in England (2020/21) (%)"
        ) %>%
        highcharter::hc_subtitle(
          text = "Note: This excludes individuals without either an age band or gender."
        ) %>%
        highcharter::hc_xAxis(
          categories = sort(unique(plot_df$AGE_BAND)),
          reversed = FALSE
        ) %>%
        highcharter::hc_yAxis(
          min = -ceiling(max_p / 5) * 5,
          max = ceiling(max_p / 5) * 5,
          labels = list(
            formatter = highcharter::JS("function(){ return Math.abs(this.value) + '%' ;}")
          )
        ) %>%
        highcharter::hc_tooltip(
          shared = FALSE,
          formatter = highcharter::JS("function () { return '<b>Gender: </b>' + this.series.name + '<br>' + '<b>Age band (5 years): </b>' + this.point.category + '<br/>' + '<b>Percentage: </b>' + Math.abs(Math.round(this.point.y * 10) / 10) + '%';}")
        )
    } else {
      highcharter::highchart() %>%
        highcharter::hc_chart(type = "bar", marginBottom = 100) %>%
        highcharter::hc_add_series_list(x = plot_series_list_count) %>%
        highcharter::hc_motion(
          labels = unique(plot_df_count$YEAR_MONTH),
          series = c(0, 1)
        ) %>%
        theme_nhsbsa(palette = "gender") %>%
        highcharter::hc_title(
          text = "Age band and gender of estimated care home residents in England (2020/21) (Count)"
        ) %>%
        highcharter::hc_subtitle(
          text = "Note: This excludes individuals without either an age band or gender."
        ) %>%
        highcharter::hc_xAxis(
          categories = sort(unique(plot_df_count$AGE_BAND)),
          reversed = FALSE
        ) %>%
        highcharter::hc_yAxis(
          min = -ceiling(max_total_pat / 5) * 5,
          max = ceiling(max_total_pat / 5) * 5,
          labels = list(
            formatter = highcharter::JS("function(){ return Math.abs(this.value) / 1000 + 'k' ;}")
          )
        ) %>%
        highcharter::hc_tooltip(
          shared = FALSE,
          formatter = highcharter::JS("function () { return '<b>Gender: </b>' + this.series.name + '<br>' + '<b>Age band (5 years): </b>' + this.point.category + '<br/>' + '<b>Number of care home residents: </b>' + Math.abs(Math.round(this.point.y / 500) * 500 / 1000) + 'k';}")
        )
    }
  })
}














# Pyramid plot for age band and gender
# output$patients_by_gender_and_age_band_chart <- highcharter::renderHighchart({
#   highcharter::highchart() %>%
#     highcharter::hc_chart(type = "bar", marginBottom = 100) %>%
#     highcharter::hc_add_series_list(x = plot_series_list) %>%
#     highcharter::hc_motion(
#       labels = unique(plot_df$YEAR_MONTH),
#       series = c(0, 1)
#     ) %>%
#     theme_nhsbsa(palette = "gender") %>%
#     highcharter::hc_title(
#       text = "Age band and gender of estimated care home residents in England (2020/21)"
#     ) %>%
#     highcharter::hc_subtitle(
#       text = "Note: This excludes individuals without either an age band or gender."
#     ) %>%
#     highcharter::hc_xAxis(
#       categories = sort(unique(plot_df$AGE_BAND)),
#       reversed = FALSE
#     ) %>%
#     highcharter::hc_yAxis(
#       min = -ceiling(max_p / 5) * 5,
#       max = ceiling(max_p / 5) * 5,
#       labels = list(
#         formatter = highcharter::JS("function(){ return Math.abs(this.value) + '%' ;}")
#       )
#     ) %>%
#     highcharter::hc_tooltip(
#       shared = FALSE,
#       formatter = highcharter::JS("function () { return '<b>Gender: </b>' + this.series.name + '<br>' + '<b>Age band (5 years): </b>' + this.point.category + '<br/>' + '<b>Percentage: </b>' + Math.abs(Math.round(this.point.y * 10) / 10) + '%';}")
#     )
# })
# }

## To be copied in the UI
# mod_patients_by_gender_and_age_band_chart_ui("patients_by_gender_and_age_band_chart_1")

## To be copied in the server
# callModule(mod_patients_by_gender_and_age_band_chart_server, "patients_by_gender_and_age_band_chart_1")
