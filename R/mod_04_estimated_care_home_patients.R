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
        style = "margin-bottom: 0;",
        selectInput(
          inputId = ns("geography"),
          label = "Geography",
          choices = c("Region", "STP", "Local Authority"),
          width = "100%"
        )
      ),
      col_6(
        style = "margin-bottom: 0;",
        selectInput(
          inputId = ns("metric"),
          label = "Metric",
          choices = c(
            "Total drug cost" =
              "SDC_COST_PER_PATIENT",
            "Number of prescription items" =
              "SDC_ITEMS_PER_PATIENT",
            "Number of unique medicines" =
              "SDC_UNIQUE_MEDICINES_PER_PATIENT",
            "Patients on ten or more unique medicines" =
              "SDC_PCT_PATIENTS_TEN_OR_MORE"
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
    ),
    mod_download_ui(
      id = ns("download_map_chart")
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
    combined_df <-
      dplyr::full_join(
        x = careHomePrescribingScrollytellR::items_and_cost_per_patient_by_breakdown_and_ch_flag_df,
        y = careHomePrescribingScrollytellR::unique_medicines_per_patient_by_breakdown_and_ch_flag_df
      )

    # Only interested in care homes and geographical breakdowns
    combined_df <- combined_df %>%
      dplyr::filter(
        grepl("Geographical - ", BREAKDOWN),
        CH_FLAG == "Care home"
      ) %>%
      dplyr::mutate(BREAKDOWN = gsub("Geographical - ", "", BREAKDOWN))

    # Rename the cols
    combined_df <- combined_df %>%
      dplyr::rename(
        GEOGRAPHY = BREAKDOWN,
        SUB_GEOGRAPHY_NAME = SUB_BREAKDOWN_NAME,
        SUB_GEOGRAPHY_CODE = SUB_BREAKDOWN_CODE
      )

    # Filter the data based on the geography
    combined_geography_df <- reactive({
      req(input$geography)

      combined_df %>%
        dplyr::filter(GEOGRAPHY == input$geography)
    })

    # Pull the metric we are interested in
    metric_df <- reactive({
      req(input$geography)
      req(input$metric)

      combined_geography_df() %>%
        dplyr::mutate(
          TOTAL_PATIENTS = switch(input$metric,
            "PCT_PATIENTS_TEN_OR_MORE" = PATIENTS_TEN_OR_MORE,
            TOTAL_PATIENTS
          )
        ) %>%
        dplyr::select(
          dplyr::all_of(
            c(
              "YEAR_MONTH",
              "GEOGRAPHY",
              "SUB_GEOGRAPHY_NAME",
              "SUB_GEOGRAPHY_CODE",
              "TOTAL_PATIENTS", # For SDC
              input$metric
            )
          )
        )
    })

    # Filter out unknown sub geographys for the plot
    plot_df <- reactive({
      req(input$geography)
      req(input$metric)

      metric_df() %>%
        dplyr::filter(!is.na(SUB_GEOGRAPHY_NAME))
    })

    # Swap NAs for "c" for data download
    download_df <- reactive({
      req(input$geography)
      req(input$metric)

      plot_df() %>%
        dplyr::mutate(
          "{input$metric}" := ifelse(
            test = is.na(.data[[input$metric]]) & TOTAL_PATIENTS > 0,
            yes = "c",
            no = as.character(.data[[input$metric]])
          )
        ) %>%
        dplyr::select(-TOTAL_PATIENTS)
    })

    # Add a download button
    mod_download_server(
      id = "download_map_chart",
      filename = "map.csv",
      export_data = download_df()
    )

    # Filter the map data based on the breakdown and format for the plot
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

      min(plot_df()[[input$metric]], na.rm = TRUE)
    })

    # Pull the max value
    max_value <- reactive({
      req(input$geography)
      req(input$metric)

      max(plot_df()[[input$metric]], na.rm = TRUE)
    })

    # Format for highchater animation
    plot_sequence_series <- reactive({
      req(input$geography)
      req(input$metric)

      # Create series (including code and name)
      plot_df() %>%
        dplyr::rename(value = .data[[input$metric]]) %>%
        dplyr::group_by(SUB_GEOGRAPHY_CODE) %>%
        dplyr::do(sequence = .$value) %>%
        dplyr::left_join(y = plot_df()) %>%
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
          joinBy = "SUB_GEOGRAPHY_CODE",
          tooltip = list(
            headerFormat = "",
            pointFormat = paste0(
              "<b>", input$geography, ":</b> {point.SUB_GEOGRAPHY_NAME}<br><b>",
              switch(input$metric,
                "SDC_COST_PER_PATIENT" =
                  "Total drug cost:</b> £{point.value}",
                "SDC_ITEMS_PER_PATIENT" =
                  "Number of prescription items:</b> {point.value}",
                "SDC_UNIQUE_MEDICINES_PER_PATIENT" =
                  "Number of unique medicines:</b> {point.value}",
                "SDC_PCT_PATIENTS_TEN_OR_MORE" =
                  "Patients on ten or more unique medicines:</b> {point.value}%"
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
