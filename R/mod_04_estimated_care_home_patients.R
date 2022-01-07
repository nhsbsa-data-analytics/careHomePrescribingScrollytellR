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
    h6(
      "The estimated average monthly drug cost for older care home patients ",
      "is around twice that for non-care home patients"
    ),
    p(
      "Overall, older care home patients receive around 1.4 times more ",
      "prescription items and unique medicines per patient per month than ",
      "non-care home older patients. At around twice the drug cost. These ",
      "prescribing metrics vary by age, gender and geography. The chart below ",
      "allows you to explore them."
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
    ),
    br(),
    h6(
      "The estimated average monthly drug cost is highest for care home ",
      "patients aged 65 to 69 years"
    ),
    p(
      "Average drug costs and volumes per patient per month are higher for ",
      "care home patients than non-care home patients across all age and ",
      "gender groups except females aged 90+ years, where items per patient ",
      "is marginally higher among non-care home patients. The greatest ",
      "differences are between younger care home and non-care home patients, ",
      "aged 65 to 69 years."
    ),
    p(
      "Among care home patients, average drug costs per patient per month ",
      "decrease as age increases, whereas the reverse is true for non-care ",
      "home patients. They are highest for the males and female care home ",
      "patients aged 65 to 69 years, and over 1.5 times higher than for 90+ ",
      "year care home patients. Drug costs are also higher for male care home ",
      "patients than females in all age groups. "
    ),
    p(
      "The number of prescription items and are broadly similar by age and ",
      "gender, although there is a smaller proportion of care home patients ",
      "aged 90+ years on 10 more drugs than other age groups. And this group ",
      "is lowest on all four prescribing metrics, with females being lower ",
      "than males."
    ),
    p("These patterns can be seen in the charts below."),
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;",
      radioButtons(
        inputId = ns("age_gender_by_metric"),
        label = NULL,
        choices = c(
          "Average Drug Cost" = "SDC_COST_PER_PATIENT",
          "Average Prescription Items" = "SDC_ITEMS_PER_PATIENT",
          "Number of unique medicines" = "SDC_UNIQUE_MEDICINES_PER_PATIENT",
          "Patients on ten or more unique medicines" = "SDC_PCT_PATIENTS_TEN_OR_MORE"
        ),
        inline = TRUE,
        width = "100%"
      ),
      br(),
      highcharter::highchartOutput(
        outputId = ns("patient_by_gender_and_age_band_and_ch_flag_chart"),
        height = "700px"
      )
    ),
    mod_download_ui(
      id = ns("download_patient_by_gender_and_age_band_and_ch_flag_chart")
    )
  )
}

#' 04_estimated_care_home_patients Server Functions
#'
#' @noRd
mod_04_estimated_care_home_patients_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Map

    # Join the metrics together
    combined_map_df <-
      dplyr::full_join(
        x = careHomePrescribingScrollytellR::items_and_cost_per_patient_by_breakdown_and_ch_flag_df,
        y = careHomePrescribingScrollytellR::unique_medicines_per_patient_by_breakdown_and_ch_flag_df
      )

    # Only interested in care homes and geographical breakdowns
    combined_map_df <- combined_map_df %>%
      dplyr::filter(
        grepl("Geographical - ", BREAKDOWN),
        CH_FLAG == "Care home"
      ) %>%
      dplyr::mutate(BREAKDOWN = gsub("Geographical - ", "", BREAKDOWN))

    # Rename the cols
    combined_map_df <- combined_map_df %>%
      dplyr::rename(
        GEOGRAPHY = BREAKDOWN,
        SUB_GEOGRAPHY_NAME = SUB_BREAKDOWN_NAME,
        SUB_GEOGRAPHY_CODE = SUB_BREAKDOWN_CODE
      )

    # Filter the data based on the geography
    combined_map_geography_df <- reactive({
      req(input$geography)

      combined_map_df %>%
        dplyr::filter(GEOGRAPHY == input$geography)
    })

    # Pull the metric we are interested in
    metric_map_df <- reactive({
      req(input$geography)
      req(input$metric)

      combined_map_geography_df() %>%
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
    plot_map_df <- reactive({
      req(input$geography)
      req(input$metric)

      metric_map_df() %>%
        dplyr::filter(!is.na(SUB_GEOGRAPHY_NAME))
    })

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

    # Pull the min value
    min_value <- reactive({
      req(input$geography)
      req(input$metric)

      min(plot_map_df()[[input$metric]], na.rm = TRUE)
    })

    # Pull the max value
    max_value <- reactive({
      req(input$geography)
      req(input$metric)

      max(plot_map_df()[[input$metric]], na.rm = TRUE)
    })

    # Format for highchater animation
    plot_map_sequence_series <- reactive({
      req(input$geography)
      req(input$metric)

      # Create series (including code and name)
      plot_map_df() %>%
        dplyr::rename(value = .data[[input$metric]]) %>%
        dplyr::group_by(SUB_GEOGRAPHY_CODE) %>%
        dplyr::do(sequence = .$value) %>%
        dplyr::left_join(y = plot_map_df()) %>%
        highcharter::list_parse()
    })

    # Create plot
    output$map_chart <- highcharter::renderHighchart({
      req(input$geography)
      req(input$metric)

      highcharter::highchart(type = "map") %>%
        highcharter::hc_chart(marginBottom = 100) %>%
        highcharter::hc_add_series(
          data = plot_map_sequence_series(),
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
        highcharter::hc_motion(labels = unique(plot_map_df()$YEAR_MONTH)) %>%
        theme_nhsbsa() %>%
        highcharter::hc_colorAxis(min = min_value(), max = max_value()) %>%
        highcharter::hc_mapNavigation(
          enabled = TRUE,
          enableMouseWheelZoom = TRUE,
          enableDoubleClickZoom = TRUE
        )
    })

    # Cost per patient by gender and age band and care home flag line chart

    # Combine the care home flag and gender into a joint metric

    combined_df <-
      dplyr::full_join(
        x = careHomePrescribingScrollytellR::items_and_cost_per_patient_by_gender_and_age_band_and_ch_flag_df %>%
          dplyr::mutate(CH_FLAG_AND_GENDER = paste(CH_FLAG, GENDER, sep = " - ")),
        y = careHomePrescribingScrollytellR::unique_medicines_per_patient_by_gender_and_age_band_and_ch_flag_df %>%
          dplyr::mutate(CH_FLAG_AND_GENDER = paste(CH_FLAG, GENDER, sep = " - "))
      )

    # Pull the metric we are interested in
    age_gender_by_metric_df <- reactive({
      req(input$age_gender_by_metric)

      combined_df %>%
        dplyr::filter(!is.na(GENDER)) %>%
        dplyr::select(
          dplyr::all_of(c("GENDER", "AGE_BAND", "CH_FLAG", "CH_FLAG_AND_GENDER", input$age_gender_by_metric))
        ) %>%
        dplyr::mutate(value = .data[[input$age_gender_by_metric]])
    })

    # Swap NAs for "c" for data download
    age_gender_by_metric_download_df <- reactive({
      req(input$age_gender_by_metric)
      
      age_gender_by_metric_df() %>% 
        dplyr::select(-c(CH_FLAG_AND_GENDER,value)) %>% 
        dplyr::mutate(
          "{input$age_gender_by_metric}" := ifelse(
            test = is.na(.data[[input$age_gender_by_metric]]),
            yes = "c",
            no = as.character(.data[[input$age_gender_by_metric]])
          )
        )
    })
    
    mod_download_server(
      id = "download_patient_by_gender_and_age_band_and_ch_flag_chart",
      filename = "patient_by_gender_and_age_band_and_ch_flag_chart.csv",
      export_data = age_gender_by_metric_download_df()
    )
    
    # Pull NAs from the selected metric,
    # aggregation is different depends on the metric
    age_gender_by_metric_na <- reactive({
      req(input$age_gender_by_metric)

      df <- combined_df %>%
        dplyr::filter(is.na(GENDER)) %>%
        dplyr::summarise(
          UNKNOWN_PAT_COUNT = round(janitor::round_half_up(sum(TOTAL_PATIENTS)),-1),
          UNKNOWN_PAT_CHAPTER_TEN = round(janitor::round_half_up(sum(TOTAL_PATIENTS_CHAPTER_TEN)),-1)) %>%
        dplyr::ungroup()

      if (input$age_gender_by_metric %in% c("SDC_COST_PER_PATIENT", "SDC_ITEMS_PER_PATIENT")) {
        return(df$UNKNOWN_PAT_COUNT)
      } else {
        return(df$UNKNOWN_PAT_CHAPTER_TEN)
      }
    })


    # Pull the min value
    min_value_line <- reactive({
      req(input$age_gender_by_metric)
      min(age_gender_by_metric_df()[[input$age_gender_by_metric]], na.rm = TRUE)
    })

    # Pull the max value
    max_value_line <- reactive({
      req(input$age_gender_by_metric)
      max(age_gender_by_metric_df()[[input$age_gender_by_metric]], na.rm = TRUE)
    })

    # had to do this way as we want to keep ch/non ch by gender
    female_ch <- fa_to_png_to_datauri(name = "female", width = 14, fill = "#003087")
    female_non_ch <- fa_to_png_to_datauri(name = "female", width = 14, fill = "#768692")
    male_ch <- fa_to_png_to_datauri(name = "male",  width = 14, fill = "#003087")
    male_non_ch <- fa_to_png_to_datauri(name = "male", width = 14, fill = "#768692")
    
    output$patient_by_gender_and_age_band_and_ch_flag_chart <- highcharter::renderHighchart({
      req(input$age_gender_by_metric)
      
      highcharter::highchart() %>% 
        highcharter::hc_add_series(
          data = age_gender_by_metric_df() %>% dplyr::filter(CH_FLAG == 'Care home' & GENDER == "Female"),
          type = "line",
          highcharter::hcaes(
            x = AGE_BAND,
            y = value
          ),
          name = "Care home - Female",
          color = "#003087",
          marker = list(symbol = stringr::str_glue("url({data_uri})", data_uri = female_ch)),
          icon = female_ch 
        ) %>% 
        highcharter::hc_add_series(
          data = age_gender_by_metric_df() %>% dplyr::filter(CH_FLAG == 'Care home' & GENDER == "Male"),
          type = "line",
          highcharter::hcaes(
            x = AGE_BAND,
            y = value
          ),
          name = "Care home - Male",
          color = "#003087",
          marker = list(symbol = stringr::str_glue("url({data_uri})", data_uri = male_ch)),
          icon = male_ch 
        ) %>% 
        highcharter::hc_add_series(
          data = age_gender_by_metric_df() %>% dplyr::filter(CH_FLAG == 'Non care home' & GENDER == "Female"),
          type = "line",
          highcharter::hcaes(
            x = AGE_BAND,
            y = value
          ),
          name = "Care home - Female",
          color = "#768692",
          marker = list(symbol = stringr::str_glue("url({data_uri})", data_uri = female_non_ch)),
          icon = female_non_ch 
        ) %>% 
        highcharter::hc_add_series(
          data = age_gender_by_metric_df() %>% dplyr::filter(CH_FLAG == 'Non care home' & GENDER == "Male"),
          type = "line",
          highcharter::hcaes(
            x = AGE_BAND,
            y = value
          ),
          name = "Care home - Male",
          color = "#768692",
          marker = list(symbol = stringr::str_glue("url({data_uri})", data_uri = male_non_ch)),
          icon = male_non_ch 
        ) %>% 
        theme_nhsbsa(stack = NA) %>%
        highcharter::hc_yAxis(
          min = min_value_line(),
          max = max_value_line(),
          title = list(
            text = paste(
              switch(input$age_gender_by_metric,
                     "SDC_COST_PER_PATIENT" = "Average drug cost",
                     "SDC_ITEMS_PER_PATIENT" = "Average number of prescription items",
                     "SDC_UNIQUE_MEDICINES_PER_PATIENT" = "Average number of unique medicines",
                     "SDC_PCT_PATIENTS_TEN_OR_MORE" = "Average % of patients prescbiring ten or more unique medicines"
              )
            )
          )
        ) %>%
        highcharter::hc_xAxis(title = list(text = "Patient Age Band")) %>%
        highcharter::hc_title(text = paste(
          "Estimated average",
          switch(input$age_gender_by_metric,
                 "SDC_COST_PER_PATIENT" = "drug cost per patient",
                 "SDC_ITEMS_PER_PATIENT" = "number of prescription items per patient",
                 "SDC_UNIQUE_MEDICINES_PER_PATIENT" = "number of unique medicines per patient",
                 "SDC_PCT_PATIENTS_TEN_OR_MORE" = "percentage of patients prescbiring ten or more unique medicines"
          ),
          "per month in England by age group and gender (2020/21)"
        )) %>%
        highcharter::hc_tooltip(
          shared = TRUE,
          headerFormat = "<b> {point.value} </b>", 
          valueSuffix = switch(input$age_gender_by_metric,
                               "SDC_PCT_PATIENTS_TEN_OR_MORE" = "%"
          ),
          valuePrefix = switch(input$age_gender_by_metric,
                               "SDC_COST_PER_PATIENT" = "£")
        ) %>%
        highcharter::hc_caption(
          text = paste0(
            "This chart excludes ", prettyNum(age_gender_by_metric_na(),big.mark = ","), " of care home patients."
          )
        ) %>% 
        highcharter::hc_credits(enabled = T)
    })
      
    
    # output$patient_by_gender_and_age_band_and_ch_flag_chart <- highcharter::renderHighchart({
    #   req(input$age_gender_by_metric)
    # 
    #   highcharter::hchart(
    #     object = age_gender_by_metric_df(),
    #     type = "line",
    #     highcharter::hcaes(
    #       x = AGE_BAND,
    #       y = value,
    #       group = CH_FLAG_AND_GENDER
    #     )
    #   ) %>%
    #     theme_nhsbsa(stack = NA) %>%
    #     highcharter::hc_yAxis(
    #       min = min_value_line(),
    #       max = max_value_line(),
    #       title = list(
    #         text = paste(
    #           switch(input$age_gender_by_metric,
    #             "SDC_COST_PER_PATIENT" = "Average drug cost",
    #             "SDC_ITEMS_PER_PATIENT" = "Average number of prescription items",
    #             "SDC_UNIQUE_MEDICINES_PER_PATIENT" = "Average number of unique medicines",
    #             "SDC_PCT_PATIENTS_TEN_OR_MORE" = "Average % of patients prescbiring ten or more unique medicines"
    #           )
    #         )
    #       )
    #     ) %>%
    #     highcharter::hc_xAxis(title = list(text = "Patient Age Band")) %>%
    #     highcharter::hc_title(text = paste(
    #       "Estimated average",
    #       switch(input$age_gender_by_metric,
    #         "SDC_COST_PER_PATIENT" = "drug cost per patient",
    #         "SDC_ITEMS_PER_PATIENT" = "number of prescription items per patient",
    #         "SDC_UNIQUE_MEDICINES_PER_PATIENT" = "number of unique medicines per patient",
    #         "SDC_PCT_PATIENTS_TEN_OR_MORE" = "percentage of patients prescbiring ten or more unique medicines"
    #       ),
    #       "per month in England by age group and gender (2020/21)"
    #     )) %>%
    #     highcharter::hc_tooltip(
    #       shared = TRUE,
    #       headerFormat = "<b> {point.value} </b>", 
    #       valueSuffix = switch(input$age_gender_by_metric,
    #                            "SDC_PCT_PATIENTS_TEN_OR_MORE" = "%"
    #                            ),
    #       valuePrefix = switch(input$age_gender_by_metric,
    #                            "SDC_COST_PER_PATIENT" = "£")
    #     ) %>%
    #     highcharter::hc_caption(
    #       text = paste0(
    #         "This chart excludes ", prettyNum(age_gender_by_metric_na(),big.mark = ","), " of care home patients."
    #       )
    #     ) %>% 
    #     highcharter::hc_credits(enabled = T) %>%
    #     highcharter::hc_colors(c("#003087", "#003087","#768692", "#768692")) 
    # })
  })
}


## To be copied in the UI
# mod_04_estimated_care_home_patients_ui("04_estimated_care_home_patients_ui_1")

## To be copied in the server
# mod_04_estimated_care_home_patients_server("04_estimated_care_home_patients_ui_1")
