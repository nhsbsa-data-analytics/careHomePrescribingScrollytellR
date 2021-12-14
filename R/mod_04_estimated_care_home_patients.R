#' 04_estimated_care_home_patients UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_04_estimated_care_home_patients_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4(
      "Estimated prescribing patterns for",
      tippy(
        text = "older care home patients",
        tooltip = tooltip_text$care_home
      )
    ),
    p(
      "Older care home patients received an estimated 35 million prescription ",
      "items at a cost of", tags$b("£320 million"), " during 2020/21."
    ),
    p(
      "Average drug costs per patient per month are highest for the youngest ",
      "care home patients amongst both males and females. They are over 1.5 ",
      "times higher for 65 to 74 year olds than 90+ year olds. Drug costs are ",
      "also higher for male care home patients than females in all age ",
      "groups. Drug volumes are broadly similar by age and gender, although ",
      "there is a smaller proportion of care home patients aged 90+ years on ",
      "10 more drugs than other age groups."
    ),
    br(),
    fluidRow(
      style = "background-color: #FFFFFF;",
      col_6(
        selectInput(
          inputId = ns("breakdown"),
          label = "Geography",
          choices = c("Region", "STP", "Local Authority"),
          width = "100%"
        )
      ),
      col_6(
        selectInput(
          inputId = ns("metric"),
          label = "Metric",
          choices = c(
            "Total drug cost" =
              "COST_PER_PATIENT",
            "Number of prescription items" =
              "ITEMS_PER_PATIENT",
            "Number of unique medicines" =
              "UNIQUE_MEDICINES_PER_PATIENT",
            "Patients on ten or more unique medicines" =
              "PCT_PATIENTS_TEN_OR_MORE"
          ),
          width = "100%"
        )
      ),
      br(),
      column(
        offset = 1,
        width = 10,
        align = "center",
        style = "background-color: #FFFFFF;",
        highcharter::highchartOutput(
          outputId = ns("map_chart"),
          height = "700px"
        )
      )
    )
  )
}

#' 04_estimated_care_home_patients Server Functions
#'
#' @noRd
mod_04_estimated_care_home_patients_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Join the metrics together
    metric_df <-
      dplyr::full_join(
        x = careHomePrescribingScrollytellR::items_and_cost_per_patient_by_breakdown_and_ch_flag_df,
        y = careHomePrescribingScrollytellR::unique_medicines_per_patient_by_breakdown_and_ch_flag_df
      )

    # Only interested in care homes and geographical breakdowns
    metric_df <- metric_df %>%
      dplyr::filter(
        grepl("Geographical - ", BREAKDOWN),
        CH_FLAG == "Care home"
      ) %>%
      dplyr::mutate(BREAKDOWN = gsub("Geographical - ", "", BREAKDOWN))

    # Filter to relevant data for this chart
    metric_df <- metric_df %>%
      dplyr::filter(dplyr::across(c(BREAKDOWN, SUB_BREAKDOWN_NAME), not_na))

    # Filter the metric data based on the breakdown and format for the plot
    plot_df <- reactive({
      req(input$breakdown)
      req(input$metric)

      metric_df %>%
        dplyr::filter(BREAKDOWN == input$breakdown) %>%
        dplyr::mutate(value = !!dplyr::sym(input$metric))
    })

    # Filter the map data based on the breakdown and format for the plot
    map_list <- reactive({
      req(input$breakdown)
      req(input$metric)

      careHomePrescribingScrollytellR::map_df %>%
        dplyr::filter(BREAKDOWN == input$breakdown) %>%
        geojsonsf::sf_geojson() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
    })

    # Pull the min value
    min_value <- reactive({
      req(input$breakdown)
      req(input$metric)

      min(abs(plot_df()$value), na.rm = TRUE)
    })

    # Pull the max value
    max_value <- reactive({
      req(input$breakdown)
      req(input$metric)

      max(abs(plot_df()$value), na.rm = TRUE)
    })

    # Format for highchater animation using tidyr::complete
    plot_sequence_series <- reactive({
      req(input$breakdown)
      req(input$metric)

      # Expand plot dataframe to cover all possibilities
      plot_df_ <- plot_df() %>%
        tidyr::complete(
          YEAR_MONTH, SUB_BREAKDOWN_CODE,
          fill = list(value = 0)
        )

      # Create series (including code and name)
      plot_df_ %>%
        dplyr::group_by(SUB_BREAKDOWN_CODE) %>%
        dplyr::do(sequence = .$value) %>%
        dplyr::left_join(y = plot_df_) %>%
        highcharter::list_parse()
    })

    # Create plot
    output$map_chart <- highcharter::renderHighchart({
      req(input$breakdown)
      req(input$metric)

      highcharter::highchart(type = "map") %>%
        highcharter::hc_chart(marginBottom = 100) %>%
        highcharter::hc_add_series(
          data = plot_sequence_series(),
          mapData = map_list(),
          joinBy = "SUB_BREAKDOWN_CODE",
          tooltip = list(
            headerFormat = "",
            pointFormat = paste0(
              "<b>", input$breakdown, ":</b> {point.SUB_BREAKDOWN_NAME}<br><b>",
              switch(input$metric,
                "COST_PER_PATIENT" =
                  "Total drug cost:</b> £{point.value:.2f}",
                "ITEMS_PER_PATIENT" =
                  "Number of prescription items:</b> {point.value:.0f}",
                "UNIQUE_MEDICINES_PER_PATIENT" =
                  "Number of unique medicines:</b> {point.value:.0f}",
                "PCT_PATIENTS_TEN_OR_MORE" =
                  "Patients on ten or more unique medicines:</b> {point.value:.0f}%"
              )
            )
          )
        ) %>%
        highcharter::hc_motion(labels = unique(plot_df()$YEAR_MONTH)) %>%
        theme_nhsbsa() %>%
        highcharter::hc_colorAxis(min = min_value(), max = max_value()) %>%
        highcharter::hc_mapNavigation(
          enabled = TRUE,
          enableMouseWheelZoom = TRUE,
          enableDoubleClickZoom = TRUE
        )
    })
  })
}

## To be copied in the UI
# mod_04_estimated_care_home_patients_ui("04_estimated_care_home_patients_ui_1")

## To be copied in the server
# mod_04_estimated_care_home_patients_server("04_estimated_care_home_patients_ui_1")
