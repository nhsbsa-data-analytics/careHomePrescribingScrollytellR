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
      "prescription items at a cost of", tags$b("£320 million"), "during ",
      "2020/21."
    ),
    h6(
      "The estimated average monthly drug cost for older care home patients ",
      "is around twice that for non-care home patients"
    ),
    p(
      "Overall, older care home patients receive around 1.4 times more ",
      "prescription items and unique medicines per patient month than ",
      "non-care home older patients. At around twice the drug cost. These ",
      "prescribing metrics vary by age, gender and geography. The chart below ",
      "allows you to explore them."
    ),
    br(),
    fluidRow(
      style = "background-color: #FFFFFF;",
      h6(
        style = "text-align: center;",
        "Estimated average monthly prescribing metrics for older care home ",
        "and non-care home patients in England by geography, age band or ",
        "gender (2020/21)"
      ),
      col_6(
        selectInput(
          inputId = ns("breakdown"),
          label = "Breakdown",
          choices = names(careHomePrescribingScrollytellR::breakdowns),
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
      uiOutput(ns("summary_table"))
    ),
    p(
      "The mean average has been used to calculate per patient month metrics. ",
      "It should be noted that the distributions are positively skewed due to ",
      "extreme high values for some patients, and the median values X are ",
      "lower than the mean."
    ),
    h6(
      "The estimated average monthly drug cost is highest for care home ",
      "patients aged 65 to 69 years"
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
      "age and gender, although there is a smaller proportion of care home ",
      "patients aged 90+ years on 10 more drugs than other age groups. And ",
      "this group is lowest on all four prescribing metrics, with females ",
      "being lower than males."
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
    ),
    p(
      "Exploratory analysis looked at possible reasons why the average drug ",
      "cost per patient month is higher for younger care home patients, but ",
      "the number of prescription items and unique medicines does not show as ",
      "much variation. "
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
    p(
      "TODO: Maybe include a version of the below chart, less non-care homes. Or a mean version with error bar? Might be a better way?"
    ),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    h6("Geography"),
    fluidRow(
      style = "background-color: #FFFFFF;",
      h6(
        style = "text-align: center;",
        "Estimated average monthly prescribing metrics for care home ",
        "patients in England by geography (2020/21)"
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
    
    # Join the 2 metric datasets together
    metric_df <-
      dplyr::full_join(
        x = careHomePrescribingScrollytellR::items_and_cost_per_patient_by_breakdown_and_ch_flag_df,
        y = careHomePrescribingScrollytellR::unique_medicines_per_patient_by_breakdown_and_ch_flag_df
      )
    
    # Overall summary boxes
    
    # Only interested in the overall period
    summary_df <- metric_df %>%
      dplyr::filter(YEAR_MONTH == "Overall")
    
    # Format cost and percentage cols
    summary_df <- summary_df %>%
      dplyr::mutate(
        SDC_COST_PER_PATIENT = paste0("£", SDC_COST_PER_PATIENT),
        SDC_PCT_PATIENTS_TEN_OR_MORE = paste0(SDC_PCT_PATIENTS_TEN_OR_MORE, "%")
      )
    
    # Handy resource: https://mastering-shiny.org/action-dynamic.html
    
    # Filter the data based on the breakdown
    summary_breakdown_df <- reactive({
      
      req(input$breakdown)
      
      summary_df %>%
        dplyr::filter(BREAKDOWN == input$breakdown)
      
    })
    
    # Pull the text for the number of NA sub breakdown patients
    patients_in_na_sub_breakdown_text <- reactive({
      
      req(input$breakdown)
      
      # Filter to NA sub breakdown
      na_sub_breakdown_df <- summary_breakdown_df() %>%
        dplyr::filter(is.na(SUB_BREAKDOWN_NAME))
      
      # Apply SDC to total patients
      na_sub_breakdown_df <- na_sub_breakdown_df %>%
        dplyr::mutate(
          SDC = ifelse(TOTAL_PATIENTS %in% c(1, 2, 3, 4), 1, 0),
          SDC_TOTAL_PATIENTS =
            ifelse(SDC == 1, NA_integer_, round(TOTAL_PATIENTS, -1))
        )
      
      # Format the number
      na_sub_breakdown_df <- na_sub_breakdown_df %>%
        dplyr::mutate(
          SDC_TOTAL_PATIENTS = ifelse(
            test = is.na(SDC_TOTAL_PATIENTS),
            yes = "c",
            no = as.character(SDC_TOTAL_PATIENTS)
          )
        )
      
      # Extract the values
      patients_in_na_sub_breakdown <- na_sub_breakdown_df %>%
        dplyr::pull(SDC_TOTAL_PATIENTS, CH_FLAG)
      
      if (length(patients_in_na_sub_breakdown) == 2) {
        # If both care home and non care home exist
        
        paste(
          "This excludes", patients_in_na_sub_breakdown[1], "care home and",
          patients_in_na_sub_breakdown[2], "non care home patients with an ",
          "unknown sub breakdown."
        )
        
      } else if (length(patients_in_na_sub_breakdown) == 1) {
        # If only one exists
        
        paste(
          "This excludes", patients_in_na_sub_breakdown,
          tolower(names(patients_in_na_sub_breakdown)), "patients with an ",
          "unknown sub breakdown."
        )
        
      } else {
        # Nothing
        
        ""
        
      }
      
    })
    
    # Update the list of choices for sub breakdown from the rows in breakdown
    # dataframe
    observeEvent(
      eventExpr = summary_breakdown_df(),
      handlerExpr = {
        freezeReactiveValue(input, "sub_breakdown")
        updateSelectInput(
          inputId = "sub_breakdown",
          choices = summary_breakdown_df()$SUB_BREAKDOWN_NAME %>%
            na.omit() %>%
            unique()
        )
      }
    )
    
    # Filter the data based on the sub breakdown
    summary_table_df <- reactive({
      
      req(input$sub_breakdown)
      
      summary_breakdown_df() %>%
        dplyr::filter(SUB_BREAKDOWN_NAME == input$sub_breakdown)
      
    })
    
    # Create the table
    output$summary_table <- renderUI({
      
      req(input$sub_breakdown)
      
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
              value = summary_table_df() %>%
                dplyr::filter(CH_FLAG == "Care home") %>%
                dplyr::pull(SDC_COST_PER_PATIENT),
              icon = "coins"
            )
          ),
          col_3(
            mod_value_box_ui(
              id = "4",
              care_home = FALSE,
              value = summary_table_df() %>%
                dplyr::filter(CH_FLAG == "Non care home") %>%
                dplyr::pull(SDC_COST_PER_PATIENT),
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
              value = summary_table_df() %>%
                dplyr::filter(CH_FLAG == "Care home") %>%
                dplyr::pull(SDC_ITEMS_PER_PATIENT),
              icon = "prescription"
            ),
          ),
          col_3(
            mod_value_box_ui(
              id = "2",
              care_home = FALSE,
              value = summary_table_df() %>%
                dplyr::filter(CH_FLAG == "Non care home") %>%
                dplyr::pull(SDC_ITEMS_PER_PATIENT),
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
              value = summary_table_df() %>%
                dplyr::filter(CH_FLAG == "Care home") %>%
                dplyr::pull(SDC_UNIQUE_MEDICINES_PER_PATIENT),
              icon = "pills"
            )
          ),
          col_3(
            mod_value_box_ui(
              id = "6",
              care_home = FALSE,
              value = summary_table_df() %>%
                dplyr::filter(CH_FLAG == "Non care home") %>%
                dplyr::pull(SDC_UNIQUE_MEDICINES_PER_PATIENT),
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
              value = summary_table_df() %>%
                dplyr::filter(CH_FLAG == "Care home") %>%
                dplyr::pull(SDC_PCT_PATIENTS_TEN_OR_MORE),
              icon = "pills"
            )
          ),
          col_3(
            mod_value_box_ui(
              id = "8",
              care_home = FALSE,
              value = summary_table_df() %>%
                dplyr::filter(CH_FLAG == "Non care home") %>%
                dplyr::pull(SDC_PCT_PATIENTS_TEN_OR_MORE),
              icon = "pills"
            )
          )
        ),
        fluidRow(
          col_12(
            p(
              style = "font-size: 12px; text-align: right; padding-right: 15px;",
              patients_in_na_sub_breakdown_text()
            )
          )
        )
      )
      
    })
    
    # Cost per patient by gender and age band and care home flag line chart
    
    # Combine the care home flag and gender into a joint metric
    cost_per_patient_by_gender_and_age_band_and_ch_flag_df <- 
      careHomePrescribingScrollytellR::cost_per_patient_by_gender_and_age_band_and_ch_flag_df %>%
      dplyr::mutate(CH_FLAG_AND_GENDER = paste(CH_FLAG, GENDER, sep = " - "))
    
    # Filter out unknown genders for the plot
    plot_line_df <- reactive({
      
      cost_per_patient_by_gender_and_age_band_and_ch_flag_df %>%
        dplyr::filter(!is.na(GENDER))
      
    })
    
    # Create chart
    output$cost_per_patient_by_gender_and_age_band_and_ch_flag_chart <- 
      highcharter::renderHighchart({
        
        highcharter::hchart(
          object = plot_line_df(),
          type = "line",
          highcharter::hcaes(
            x = AGE_BAND, 
            y = SDC_COST_PER_PATIENT, 
            group = CH_FLAG_AND_GENDER
          )
        ) %>% 
          highcharter::hc_yAxis(
            min = 0,
            max = 150,
            title = list(text = "Mean Cost per Patient (£)")
          ) %>% 
          highcharter::hc_xAxis(title = list(text = "Patient Age Band")) %>% 
          highcharter::hc_title(text = "Mean Care Home Patient Drug Cost per 
        Age Band for the Financial Year 2020/21") %>% 
          highcharter::hc_tooltip(
            pointFormat = "<b>{point.CH_FLAG_AND_GENDER}:</b> £{point.COST_PER_PATIENT:,.2f}"
          ) %>% 
          highcharter::hc_credits(enabled = T) %>% 
          highcharter::hc_colors(c("darkgreen", "lightgreen", "darkblue", "lightblue"))
        
      })
    
    # Map
    
    # Only interested in care homes and geographical breakdowns
    map_df <- metric_df %>%
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
          TOTAL_PATIENTS = switch(
            input$metric,
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
      
      map_metric_df() %>%
        dplyr::filter(!is.na(SUB_GEOGRAPHY_NAME))
      
    })
    
    # Create a table of the top 5 and bottom 5 results in the overall month to 
    # go alongside the map
    output$map_table <- DT::renderDataTable(
      
      expr = {
        
        req(input$geography)
        req(input$metric)
        
        # Get a nice name for the metric
        nice_metric_name <- switch(
          input$metric,
          "SDC_COST_PER_PATIENT" = "Total drug cost (£)",
          "SDC_ITEMS_PER_PATIENT" = "Number of prescription items",
          "SDC_UNIQUE_MEDICINES_PER_PATIENT" = "Number of unique medicines",
          "SDC_PCT_PATIENTS_TEN_OR_MORE" = 
            "Patients on ten or more unique medicines (%)"
        )
        
        # Format the table
        plot_map_df() %>%
          dplyr::filter(YEAR_MONTH == "Overall") %>%
          dplyr::arrange(desc(.data[[input$metric]])) %>%
          dplyr::select(SUB_GEOGRAPHY_NAME, .data[[input$metric]]) %>%
          dplyr::rename(
            "{input$geography}" := SUB_GEOGRAPHY_NAME,
            "{nice_metric_name}" := .data[[input$metric]]
          )
        
      },
      
      options = list(dom = "t", scrollCollapse = TRUE, paging = FALSE, scrollY = '500px'),
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
              switch(
                input$metric,
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
    
    
    
    
    
  })
}

## To be copied in the UI
# mod_03_care_home_prescribing_ui("03_care_home_prescribing_ui_1")

## To be copied in the server
# mod_03_care_home_prescribing_server("03_care_home_prescribing_ui_1")