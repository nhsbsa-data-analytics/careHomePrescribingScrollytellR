#' 03_care_home_prescribing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_03_care_home_prescribing_ui <- function(id) {
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
      "Older care home patients received an estimated", tags$b("35 million"),
      "prescription items at a cost of", tags$b("£324 million"), "during ",
      "2020/21."
    ),
    p(
      "This represents 7% of the total primary care drug spend for older ",
      "patients during 2020/21."
    ),
    h6(
      "The estimated average monthly drug cost for older care home patients ",
      "is around twice that for older non-care home patients who received ",
      "prescriptions."
    ),
    p(
      "We estimate that older care home patients receive around 1.6 times ",
      "more prescription items and unique medicines per patient month than ",
      "older non-care home patients who received prescriptions. At around ",
      "twice the drug cost. These prescribing metrics vary by age, gender and ",
      "geography. The chart below allows you to explore them."
    ),
    br(),
    fluidRow(
      style = "background-color: #FFFFFF;",
      h6(
        style = "text-align: center;",
        "Estimated average prescribing metrics per patient month for older ",
        "care home and non-care home patients in England by geography, age ",
        "band or gender (2020/21)"
      ),
      col_6(
        selectInput(
          inputId = ns("breakdown"),
          label = "Breakdown",
          choices = names(careHomePrescribingScrollytellR::breakdowns) %>%
            purrr::discard(
              .p = stringr::str_detect(
                string = .,
                pattern = "Additional - "
              )
            ),
          width = "100%"
        )
      ),
      col_6(
        selectInput(
          inputId = ns("sub_breakdown"),
          label = "Sub Breakdown",
          choices = NULL, # dynamically generated
          width = "100%"
        )
      ),
      br(),
      fluidRow(
        col_6(
          style = "text-indent: 15px;",
          h6("Metric")
        ),
        col_3(
          style = "text-align: center;",
          h6("Care home")
        ),
        col_3(
          style = "text-align: center;",
          h6("Non-care home")
        )
      ),
      uiOutput(ns("care_home_vs_non_care_home_table"))
    ),
    
    
    
    
    br(),
    br(),
    h6("Age and gender"),
    h6(
      "The estimated average drug cost per patient month is highest for ",
      "care home patients aged 65 to 69 years."
    ),
    p(
      "Average drug costs and volumes per patient month are higher for care ",
      "home patients than non-care home patients across all age and gender ",
      "groups except females aged 90+ years, where items per patient is ",
      "marginally higher among non-care home patients. The greatest ",
      "differences are between younger care home and non-care home patients, ",
      "aged 65 to 69 years."
    ),
    p(
      "Among care home patients, average drug costs per patient month ",
      "decrease as age increases, whereas the reverse is true for non-care ",
      "home patients. They are highest for the males and female care home ",
      "patients aged 65 to 69 years, and over 1.5 times higher than for 90+ ",
      "year care home patients. Drug costs are also higher for male care home ",
      "patients than females in all age groups."
    ),
    p(
      "The average number of prescription items, unique medicines and ",
      "percentage of patients on 10 or more medicines are broadly similar by ",
      "age and gender among care home patients, although there is a smaller ",
      "proportion of care home patients aged 90+ years on 10 more drugs than ",
      "other age groups. And this group is lowest on all four prescribing ",
      "metrics, with females being lower than males."
    ),
    p("These patterns can be seen in the charts below."),
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;",
      h6(
        "Estimated average prescribing metrics per patient month for older ",
        "care home and non-care home patients in England by age band and ",
        "gender (2020/21)"
      ),
      radioButtons(
        inputId = ns("gender_and_age_band_and_ch_flag_metric"),
        label = NULL,
        choices = c(
          "Drug cost" = "SDC_COST_PER_PATIENT_MONTH",
          "Number of prescription items" = "SDC_ITEMS_PER_PATIENT_MONTH",
          "Number of unique medicines" =
            "SDC_UNIQUE_MEDICINES_PER_PATIENT_MONTH",
          "Patients on ten or more unique medicines" =
            "SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH"
        ),
        inline = TRUE,
        width = "100%"
      ),
      highcharter::highchartOutput(
        outputId = ns("metrics_by_gender_and_age_band_and_ch_flag_chart"),
        height = "350px"
      )
    ),
    mod_download_ui(
      id = ns("download_metrics_by_gender_and_age_band_and_ch_flag_chart")
    ),
    br(),
    br(),
    p(
      "Exploratory analysis looked at possible reasons why the average drug ",
      "cost per patient month is higher for younger care home patients, but ",
      "the number of prescription items and unique medicines does not show as ",
      "much variation."
    ),
    p("Analysis indicates that it may be a combination of factors including:"),
    tags$ul(
      tags$li(
        style = "font-size: 16pt;",
        "Younger age groups have a much wider spread of costs per month and ",
        "include more outliers."
      ),
      tags$li(
        style = "font-size: 16pt;",
        "Younger care home patients seem to have more prescribing of ",
        "appliances and a larger spend on nutrition products and less ",
        "prescribing of cardiovascular products."
      ),
      tags$li(
        style = "font-size: 16pt;",
        "Younger care home patients typically get a larger quantity than ",
        "older care home patients when prescribed comparable drugs/products."
      )
    ),
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;",
      h6(
        "Distribution of the estimated average drug cost per patient month",
        "for older care home patients in England (2020/21)"
      ),
      highcharter::highchartOutput(
        outputId = ns("cost_per_patient_by_age_band_chart"),
        height = "300px"
      )
    ),
    br(),
    br(),
    h6("Geography"),
    h6(
      "The London region has the highest estimated average prescribing costs ",
      "and volumes per patient month."
    ),
    p(
      "The London region features the highest average rate per patient month ",
      "on all four prescribing metrics and South West is lowest. There is ",
      "considerable variation per patient month by STP and local authority ",
      "across metrics, with high pockets in several London and some West ",
      "Midland STP's and local authorities."
    ),
    p(
      "Each of the metrics can be explored in the chart and table below by ",
      "region, local authority and STP."
    ),
    fluidRow(
      style = "background-color: #FFFFFF;",
      h6(
        style = "text-align: center;",
        "Estimated average prescribing metrics per patient month for older ",
        "care home patients in England by geography (2020/21)"
      ),
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
            "Drug cost" = "SDC_COST_PER_PATIENT_MONTH",
            "Number of prescription items" = "SDC_ITEMS_PER_PATIENT_MONTH",
            "Number of unique medicines" =
              "SDC_UNIQUE_MEDICINES_PER_PATIENT_MONTH",
            "Patients on ten or more unique medicines" =
              "SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH"
          ),
          width = "100%"
        )
      ),
      br(),
      col_8(
        align = "center",
        style = "background-color: #FFFFFF;",
        highcharter::highchartOutput(
          outputId = ns("map_chart"),
          height = "700px"
        )
      ),
      col_4(
        style = "background-color: #FFFFFF;",
        DT::dataTableOutput(outputId = ns("map_table"))
      )
    ),
    mod_download_ui(
      id = ns("download_map_chart")
    )
  )
}

#' 03_care_home_prescribing Server Functions
#'
#' @noRd
mod_03_care_home_prescribing_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Overall summary boxes

    # Not interested in additional metrics
    care_home_vs_non_care_home_df <-
      careHomePrescribingScrollytellR::metrics_by_breakdown_and_ch_flag_df %>%
      dplyr::filter(
        BREAKDOWN != "Additional - Gender and Age Band",
        BREAKDOWN != "Additional - Care home type"
      )

    # Format cost and percentage cols
    care_home_vs_non_care_home_df <- care_home_vs_non_care_home_df %>%
      dplyr::mutate(
        SDC_COST_PER_PATIENT_MONTH = paste0("£", SDC_COST_PER_PATIENT_MONTH),
        SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH =
          paste0(SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH, "%")
      )

    # Handy resource: https://mastering-shiny.org/action-dynamic.html

    # Filter the data based on the breakdown
    care_home_vs_non_care_home_breakdown_df <- reactive({
      req(input$breakdown)

      care_home_vs_non_care_home_df %>%
        dplyr::filter(BREAKDOWN == input$breakdown)
    })

    # Update the list of choices for sub breakdown from the rows in breakdown
    # dataframe
    observeEvent(
      eventExpr = care_home_vs_non_care_home_breakdown_df(),
      handlerExpr = {
        freezeReactiveValue(input, "sub_breakdown")
        updateSelectInput(
          inputId = "sub_breakdown",
          choices = 
            care_home_vs_non_care_home_breakdown_df()$SUB_BREAKDOWN_NAME %>%
            na.omit() %>%
            unique()
        )
      }
    )

    # Filter the data based on the sub breakdown
    care_home_vs_non_care_home_sub_breakdown_df <- reactive({
      req(input$breakdown)
      req(input$sub_breakdown)

      care_home_vs_non_care_home_breakdown_df() %>%
        dplyr::filter(SUB_BREAKDOWN_NAME == input$sub_breakdown)
    })

    # Create the table
    output$care_home_vs_non_care_home_table <- renderUI({
      req(input$breakdown)
      req(input$sub_breakdown)

      # Create table
      tagList(
        fluidRow(
          col_6(
            style = "text-indent: 15px;",
            p(
              tippy(
                text = "Drug cost",
                tooltip = tooltip_text$cost
              )
            )
          ),
          col_3(
            mod_value_box_ui(
              id = "3",
              care_home = TRUE,
              value = care_home_vs_non_care_home_sub_breakdown_df() %>%
                dplyr::filter(CH_FLAG == "Care home") %>%
                dplyr::pull(SDC_COST_PER_PATIENT_MONTH),
              icon = "coins"
            )
          ),
          col_3(
            mod_value_box_ui(
              id = "4",
              care_home = FALSE,
              value = care_home_vs_non_care_home_sub_breakdown_df() %>%
                dplyr::filter(CH_FLAG == "Non care home") %>%
                dplyr::pull(SDC_COST_PER_PATIENT_MONTH),
              icon = "coins"
            )
          )
        ),
        fluidRow(
          col_6(
            style = "text-indent: 15px;",
            p(
              tippy(
                text = "Number of prescription items",
                tooltip = tooltip_text$items
              )
            )
          ),
          col_3(
            mod_value_box_ui(
              id = "1",
              care_home = TRUE,
              value = care_home_vs_non_care_home_sub_breakdown_df() %>%
                dplyr::filter(CH_FLAG == "Care home") %>%
                dplyr::pull(SDC_ITEMS_PER_PATIENT_MONTH),
              icon = "prescription"
            ),
          ),
          col_3(
            mod_value_box_ui(
              id = "2",
              care_home = FALSE,
              value = care_home_vs_non_care_home_sub_breakdown_df() %>%
                dplyr::filter(CH_FLAG == "Non care home") %>%
                dplyr::pull(SDC_ITEMS_PER_PATIENT_MONTH),
              icon = "prescription"
            )
          )
        ),
        fluidRow(
          col_6(
            style = "text-indent: 15px;",
            p(
              tippy(
                text = "Number of unique medicines",
                tooltip = tooltip_text$unique_medicines
              )
            )
          ),
          col_3(
            mod_value_box_ui(
              id = "5",
              care_home = TRUE,
              value = care_home_vs_non_care_home_sub_breakdown_df() %>%
                dplyr::filter(CH_FLAG == "Care home") %>%
                dplyr::pull(SDC_UNIQUE_MEDICINES_PER_PATIENT_MONTH),
              icon = "pills"
            )
          ),
          col_3(
            mod_value_box_ui(
              id = "6",
              care_home = FALSE,
              value = care_home_vs_non_care_home_sub_breakdown_df() %>%
                dplyr::filter(CH_FLAG == "Non care home") %>%
                dplyr::pull(SDC_UNIQUE_MEDICINES_PER_PATIENT_MONTH),
              icon = "pills"
            )
          )
        ),
        fluidRow(
          col_6(
            style = "text-indent: 15px;",
            p(
              tippy(
                text = "Patients on ten or more unique medicines",
                tooltip = tooltip_text$ten_or_more_unique_medicines
              )
            )
          ),
          col_3(
            mod_value_box_ui(
              id = "7",
              care_home = TRUE,
              value = care_home_vs_non_care_home_sub_breakdown_df() %>%
                dplyr::filter(CH_FLAG == "Care home") %>%
                dplyr::pull(SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH),
              icon = "pills"
            )
          ),
          col_3(
            mod_value_box_ui(
              id = "8",
              care_home = FALSE,
              value = care_home_vs_non_care_home_sub_breakdown_df() %>%
                dplyr::filter(CH_FLAG == "Non care home") %>%
                dplyr::pull(SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH),
              icon = "pills"
            )
          )
        ),
        fluidRow(
          col_12(
            class = "highcharts-caption",
            style = "margin-left: 1%; margin-right: 1%;",
            switch(input$breakdown,
              "Demographical - Gender" = "This excludes << 1% of patients with an unknown gender and where the number of patients is less than 5 the data has been redacted.",
              "Where the number of patients is less than 5 the data has been redacted."
            ),
            "The mean average has been used to calculate per patient month ",
            "metrics. It should be noted that the distributions are ",
            "positively skewed due to extreme high values for some patients, ",
            "and median values are lower than the mean.",
            br()
          )
        )
      )
    })

    # Cost per patient by gender and age band and care home flag line chart

    # Interested in the gender and age band metric
    metrics_by_gender_and_age_band_and_ch_flag_df <-
      careHomePrescribingScrollytellR::metrics_by_breakdown_and_ch_flag_df %>%
      dplyr::filter(BREAKDOWN == "Additional - Gender and Age Band")

    # Filter out the NA genders
    metrics_by_gender_and_age_band_and_ch_flag_df_ <- reactive({
      req(input$gender_and_age_band_and_ch_flag_metric)

      metrics_by_gender_and_age_band_and_ch_flag_df %>%
        dplyr::filter(!is.na(GENDER))
    })

    # Swap NAs for "c" for data download and subset columns
    metrics_by_gender_and_age_band_and_ch_flag_download_df <- reactive({
      req(input$gender_and_age_band_and_ch_flag_metric)

      metrics_by_gender_and_age_band_and_ch_flag_df_() %>%
        dplyr::mutate(
          "{input$gender_and_age_band_and_ch_flag_metric}" := ifelse(
            test = is.na(.data[[input$gender_and_age_band_and_ch_flag_metric]]),
            yes = "c",
            no = as.character(.data[[input$gender_and_age_band_and_ch_flag_metric]])
          )
        ) %>%
        dplyr::select(
          GENDER,
          AGE_BAND,
          CH_FLAG,
          .data[[input$gender_and_age_band_and_ch_flag_metric]]
        )
    })

    # Add data download
    mod_download_server(
      id = "download_metrics_by_gender_and_age_band_and_ch_flag_chart",
      filename = "metrics_by_gender_and_age_band_and_ch_flag_chart.csv",
      export_data = metrics_by_gender_and_age_band_and_ch_flag_download_df()
    )

    # Manually define the icons
    female_ch <- fa_to_png_to_datauri(
      name = "female",
      width = 13,
      fill = "#003087"
    )
    female_non_ch <- fa_to_png_to_datauri(
      name = "female",
      width = 13,
      fill = "#768692"
    )
    male_ch <- fa_to_png_to_datauri(
      name = "male",
      width = 10,
      fill = "#003087"
    )
    male_non_ch <- fa_to_png_to_datauri(
      name = "male",
      width = 10,
      fill = "#768692"
    )

    # Create chart
    output$metrics_by_gender_and_age_band_and_ch_flag_chart <-
      highcharter::renderHighchart({
        req(input$gender_and_age_band_and_ch_flag_metric)

        highcharter::highchart() %>%
          highcharter::hc_add_series(
            data = metrics_by_gender_and_age_band_and_ch_flag_df_() %>%
              dplyr::filter(CH_FLAG == "Care home" & GENDER == "Female"),
            type = "line",
            highcharter::hcaes(
              x = AGE_BAND,
              y = .data[[input$gender_and_age_band_and_ch_flag_metric]]
            ),
            name = "Care home - Female",
            color = "#003087",
            marker = list(
              symbol = stringr::str_glue("url({data_uri})", data_uri = female_ch),
              radius = 2
            ),
            icon = female_ch
          ) %>%
          highcharter::hc_add_series(
            data = metrics_by_gender_and_age_band_and_ch_flag_df_() %>%
              dplyr::filter(CH_FLAG == "Care home" & GENDER == "Male"),
            type = "line",
            highcharter::hcaes(
              x = AGE_BAND,
              y = .data[[input$gender_and_age_band_and_ch_flag_metric]]
            ),
            name = "Care home - Male",
            color = "#003087",
            marker = list(
              symbol = stringr::str_glue("url({data_uri})", data_uri = male_ch),
              radius = 2
            ),
            icon = male_ch
          ) %>%
          highcharter::hc_add_series(
            data = metrics_by_gender_and_age_band_and_ch_flag_df_() %>%
              dplyr::filter(CH_FLAG == "Non care home" & GENDER == "Female"),
            type = "line",
            highcharter::hcaes(
              x = AGE_BAND,
              y = .data[[input$gender_and_age_band_and_ch_flag_metric]]
            ),
            name = "Non-care home - Female",
            color = "#768692",
            marker = list(
              symbol = stringr::str_glue("url({data_uri})", data_uri = female_non_ch),
              radius = 2
            ),
            icon = female_non_ch
          ) %>%
          highcharter::hc_add_series(
            data = metrics_by_gender_and_age_band_and_ch_flag_df_() %>%
              dplyr::filter(CH_FLAG == "Non care home" & GENDER == "Male"),
            type = "line",
            highcharter::hcaes(
              x = AGE_BAND,
              y = .data[[input$gender_and_age_band_and_ch_flag_metric]]
            ),
            name = "Non-care home - Male",
            color = "#768692",
            marker = list(
              symbol = stringr::str_glue("url({data_uri})", data_uri = male_non_ch),
              radius = 2
            ),
            icon = male_non_ch
          ) %>%
          theme_nhsbsa(stack = NA) %>%
          highcharter::hc_yAxis(
            min = 0,
            title = list(
              text = paste(
                switch(input$gender_and_age_band_and_ch_flag_metric,
                  "SDC_COST_PER_PATIENT_MONTH" = "Drug cost (£)",
                  "SDC_ITEMS_PER_PATIENT_MONTH" =
                    "Number of prescription items",
                  "SDC_UNIQUE_MEDICINES_PER_PATIENT_MONTH" =
                    "Number of unique medicines",
                  "SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH" =
                    "Patients on ten or more unique medicines (%)"
                )
              )
            )
          ) %>%
          highcharter::hc_xAxis(
            title = list(text = "Patient Age Band"),
            categories = unique(metrics_by_gender_and_age_band_and_ch_flag_df_()$AGE_BAND)
          ) %>%
          highcharter::hc_tooltip(
            shared = TRUE,
            headerFormat = "<b> {point.value} </b>",
            valueSuffix = switch(input$gender_and_age_band_and_ch_flag_metric,
              "SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH" = "%"
            ),
            valuePrefix = switch(input$gender_and_age_band_and_ch_flag_metric,
              "SDC_COST_PER_PATIENT_MONTH" = "£"
            )
          ) %>%
          highcharter::hc_caption(
            text = "This excludes << 1% of patients with an unknown gender.",
            align = "right"
          )
      })

    # Boxplot

    # Create chart
    output$cost_per_patient_by_age_band_chart <- highcharter::renderHighchart({
      highcharter::highchart() %>%
        theme_nhsbsa() %>%
        highcharter::hc_xAxis(type = "category") %>%
        highcharter::hc_add_series_list(
          x = careHomePrescribingScrollytellR::cost_per_patient_by_age_band_series
        ) %>%
        highcharter::hc_xAxis(title = list(text = "Age band")) %>%
        highcharter::hc_yAxis(
          min = 0,
          title = list(text = "Drug cost (£)")
        ) %>%
        highcharter::hc_plotOptions(series = list(showInLegend = FALSE))
    })

    # Map

    # Only interested in care homes and geographical breakdowns
    map_df <-
      careHomePrescribingScrollytellR::metrics_by_breakdown_and_ch_flag_df %>%
      dplyr::filter(
        grepl("Geographical - ", BREAKDOWN),
        CH_FLAG == "Care home"
      ) %>%
      dplyr::mutate(BREAKDOWN = gsub("Geographical - ", "", BREAKDOWN))

    # Rename the cols
    map_df <- map_df %>%
      dplyr::rename(
        GEOGRAPHY = BREAKDOWN,
        SUB_GEOGRAPHY_NAME = SUB_BREAKDOWN_NAME,
        SUB_GEOGRAPHY_CODE = SUB_BREAKDOWN_CODE
      )

    # Filter the data based on the geography
    map_geography_df <- reactive({
      req(input$geography)

      map_df %>%
        dplyr::filter(GEOGRAPHY == input$geography)
    })

    # Pull the metric we are interested in
    map_metric_df <- reactive({
      req(input$geography)
      req(input$metric)

      map_geography_df() %>%
        dplyr::mutate(
          TOTAL_PATIENTS = switch(input$metric,
            "PCT_PATIENTS_TEN_OR_MORE" = PATIENTS_TEN_OR_MORE,
            TOTAL_PATIENTS
          )
        ) %>%
        dplyr::select(
          GEOGRAPHY,
          SUB_GEOGRAPHY_NAME,
          SUB_GEOGRAPHY_CODE,
          TOTAL_PATIENTS,
          .data[[input$metric]]
        )
    })

    # Filter out unknown sub geographys for the plot
    plot_map_df <- reactive({
      req(input$geography)
      req(input$metric)

      map_metric_df() %>%
        dplyr::filter(!is.na(SUB_GEOGRAPHY_NAME))
    })

    # Create a table to go alongside the map
    output$map_table <- DT::renderDataTable(
      expr = {
        req(input$geography)
        req(input$metric)

        # Get a nice name for the metric
        nice_metric_name <- switch(input$metric,
          "SDC_COST_PER_PATIENT_MONTH" = "Drug cost (£)",
          "SDC_ITEMS_PER_PATIENT_MONTH" = "Number of prescription items",
          "SDC_UNIQUE_MEDICINES_PER_PATIENT_MONTH" = "Number of unique medicines",
          "SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH" =
            "Patients on ten or more unique medicines (%)"
        )

        # Format the table
        plot_map_df() %>%
          dplyr::arrange(desc(.data[[input$metric]])) %>%
          dplyr::select(SUB_GEOGRAPHY_NAME, .data[[input$metric]]) %>%
          dplyr::rename(
            "{input$geography}" := SUB_GEOGRAPHY_NAME,
            "{nice_metric_name}" := .data[[input$metric]]
          )
      },
      options = list(
        dom = "t",
        scrollCollapse = TRUE,
        paging = FALSE,
        scrollY = "500px"
      ),
      filter = "none"
    )


    # Swap NAs for "c" for data download
    download_map_df <- reactive({
      req(input$geography)
      req(input$metric)

      plot_map_df() %>%
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
      filename = "map_chart.csv",
      export_data = download_map_df()
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

    # Create plot
    output$map_chart <- highcharter::renderHighchart({
      req(input$geography)
      req(input$metric)

      highcharter::highchart() %>%
        highcharter::hc_add_series_map(
          df = plot_map_df(),
          map = map_list(),
          joinBy = "SUB_GEOGRAPHY_CODE",
          value = input$metric,
          tooltip = list(
            headerFormat = "",
            pointFormat = paste0(
              "<b>", input$geography, ":</b> {point.SUB_GEOGRAPHY_NAME}<br><b>",
              switch(input$metric,
                "SDC_COST_PER_PATIENT_MONTH" =
                  "Drug cost:</b> £{point.value}",
                "SDC_ITEMS_PER_PATIENT_MONTH" =
                  "Number of prescription items:</b> {point.value}",
                "SDC_UNIQUE_MEDICINES_PER_PATIENT_MONTH" =
                  "Number of unique medicines:</b> {point.value}",
                "SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH" =
                  "Patients on ten or more unique medicines:</b> {point.value}%"
              )
            )
          )
        ) %>%
        theme_nhsbsa() %>%
        highcharter::hc_caption(
          text = "Where the number of patients is less than 5 the data has been redacted."
        ) %>%
        highcharter::hc_mapNavigation(
          enabled = TRUE,
          enableMouseWheelZoom = TRUE,
          enableDoubleClickZoom = TRUE
        )
    })
  })
}

## To be copied in the UI
# mod_03_care_home_prescribing_ui("03_care_home_prescribing_ui_1")

## To be copied in the server
# mod_03_care_home_prescribing_server("03_care_home_prescribing_ui_1")
