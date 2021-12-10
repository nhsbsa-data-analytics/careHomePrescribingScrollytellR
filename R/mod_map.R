#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h4("Map"),
    fluidRow(
      style = "background-color: #FFFFFF;",
      col_6(
        selectInput(
          inputId = ns("geography"),
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
            "Total drug cost" = "COST_PER_PATIENT",
            "Number of prescription items" = "ITEMS_PER_PATIENT",
            "Number of unique medicines" = "UNIQUE_MEDICINES_PER_PATIENT",
            "Patients on ten or more unique medicines" = "PCT_PATIENTS_TEN_OR_MORE"
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

#' map Server Function
#'
#' @noRd
mod_map_server <- function(input, output, session) {
  ns <- session$ns

  # Radio button added as we cannot add two values in one sequence for the hc_motion
  metric_selection <- reactiveValues(v = NULL)

  observe({
    metric_selection$v <- input$metric
  })

  # create as reactive value - now it holds selected value
  input_metric <- reactive(metric_selection$v)

  # Join the metrics together
  metric_df <-
    dplyr::full_join(
      x = careHomePrescribingScrollytellR::items_and_cost_per_patient_by_geography_and_ch_flag_df,
      y = careHomePrescribingScrollytellR::unique_medicines_per_patient_by_geography_df
    )

  # Only interested in care homes
  metric_df <- metric_df %>%
    dplyr::filter(CH_FLAG == "Care home")

  # Filter to relevant data for this chart
  metric_df <- metric_df %>%
    dplyr::filter(dplyr::across(c(GEOGRAPHY, SUB_GEOGRAPHY_NAME), not_na))

  # Handy resource: https://mastering-shiny.org/action-dynamic.html

  # Filter the metric data based on the geography and format for the plot
  plot_df <- reactive({
    req(input$geography)
    req(input$metric)

    metric_df %>%
      dplyr::filter(GEOGRAPHY == input$geography) %>%
      dplyr::mutate(value = !!dplyr::sym(input$metric))
  })

  # Filter the map data based on the geography and format for the plot
  map_list <- reactive({
    req(input$geography)
    req(input$metric)

    careHomePrescribingScrollytellR::map_df %>%
      dplyr::filter(GEOGRAPHY == input$geography) %>%
      geojsonsf::sf_geojson() %>%
      jsonlite::fromJSON(simplifyVector = FALSE)
  })

  # Pull the min value
  min_value <- reactive({
    req(input$geography)
    req(input$metric)

    min(abs(plot_df()$value), na.rm = TRUE)
  })

  # Pull the max value
  max_value <- reactive({
    req(input$geography)
    req(input$metric)

    max(abs(plot_df()$value), na.rm = TRUE)
  })

  # Format for highchater animation
  # using tidyr::complete
  plot_sequence_series <- reactive({
    req(input$geography)
    req(input$metric)

    plot_df() %>%
      tidyr::complete(
        YEAR_MONTH, SUB_GEOGRAPHY_CODE,
        fill = list(value = 0)
      ) %>%
      dplyr::group_by(SUB_GEOGRAPHY_CODE) %>%
      dplyr::do(sequence = .$value) %>%
      highcharter::list_parse()
  })

  # Create plot
  output$map_chart <- highcharter::renderHighchart({
    req(input$geography)
    req(input$metric)

    highcharter::highchart(type = "map") %>%
      highcharter::hc_chart(marginBottom = 100) %>%
      highcharter::hc_add_series(
        data = plot_sequence_series(),
        mapData = map_list(),
        joinBy = "SUB_GEOGRAPHY_CODE"
      ) %>%
      highcharter::hc_motion(labels = unique(plot_df()$YEAR_MONTH)) %>%
      theme_nhsbsa() %>%
      highcharter::hc_colorAxis(min = min_value(), max = max_value())
  })
}

## To be copied in the UI
# mod_map_ui("map_1")

## To be copied in the server
# callModule(mod_map_server, "map_1")
