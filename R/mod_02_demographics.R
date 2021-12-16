#' 02_demographics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_demographics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4(
      "Demographic estimates for",
      tippy(
        text = "older care home patients",
        tooltip = tooltip_text$care_home
      ),
      "receiving prescriptions"
    ),
    p(
      "Overall, we estimate a monthly average of ", tags$b("285 thousand care home patients,"),
      " aged 65+ years receiving prescriptions, which accounts for around 4% of patients aged 65+ ",
      "years receiving prescription items each month."
    ),
    p(
      "Overall, the age and gender profile is broadly comparable to",
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
          inputId = ns("geography"),
          label = "Geography",
          choices = names(careHomePrescribingScrollytellR::geographys),
          width = "100%"
        )
      ),
      col_6(
        selectInput(
          inputId = ns("sub_geography"),
          label = "Sub Geography",
          choices = NULL, # dynamically generated
          width = "100%"
        )
      ),
      radioButtons(
        inputId = ns("metric"),
        label = "",
        choices = c(
          "Count" = "SDC_TOTAL_PATIENTS", 
          "Percentage" = "SDC_PCT_PATIENTS"
        ),
        inline = TRUE,
        width = "100%"
      ),
      col_8(
        highcharter::highchartOutput(
          outputId = ns("patients_by_gender_and_age_band_chart"),
          height = "500px",
          width = "900px"
        )
      ),
      col_3(
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        uiOutput(
          outputId = ns("chart_text")
        )
      )
    ),
    mod_download_ui(
      id = ns("download_patients_by_geography_and_gender_and_age_band_chart")
    ),
    br(),
    br(),
    p(
      "Based on CQC data, we estimate similar proportions of care home ",
      "patients aged 65+ living in ", tags$b("residential homes"), " (40%) ",
      "and ", tags$b("nursing homes"), " (37%) each month.", "A small ",
      "percentage (2%) appear in both settings and there are 21% who we were ",
      "unable to match against a residential or nursing home within CQC ",
      "dataset."
    ),
    p(
      "Care home patient's prescriptions were allocated an ",
      a("Index of Multiple Deprivation (IMD)",
        href = "https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019",
        target = "_blank"
      ), " and associated ",
      "decile based on the area in which the care home is located. On ",
      "average, the proportion is very close to 20% in each ",
      tags$b("IMD quintile,"),
      " which suggests equal distribution and little ",
      "variation."
    ),
    br(),
    h6("Deprivation quintile of older care home patients in England (2020/21)"),
    highcharter::highchartOutput(
      outputId = ns("imd_quintile_chart"),
      height = "400px",
      width = "900px"
    )
  )
}

#' 02_demographics Server Functions
#'
#' @noRd
mod_02_demographics_server <- function(id, export_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Handy resource: https://mastering-shiny.org/action-dynamic.html

    # Filter the data based on the geography
    geography_df <- reactive({
      
      req(input$geography)

      careHomePrescribingScrollytellR::patients_by_geography_and_gender_and_age_band_df %>%
        dplyr::filter(GEOGRAPHY == input$geography)
      
    })
    
    # Pull the number of NA sub geography patients
    patients_in_na_sub_geography <- reactive({
      
      req(input$geography)
      
      geography_df() %>%
        dplyr::filter(is.na(SUB_GEOGRAPHY)) %>%
        # Format number
        dplyr::mutate(
          SDC_TOTAL_PATIENTS = ifelse(
            test = is.na(SDC_TOTAL_PATIENTS), 
            yes = "c", 
            no = as.character(SDC_TOTAL_PATIENTS)
          )
        ) %>%
        dplyr::pull(SDC_TOTAL_PATIENTS)

    })

    # Update the list of choices for sub geography from the non NA rows in the 
    # geography dataframe
    observeEvent(
      eventExpr = geography_df(),
      handlerExpr = {
        freezeReactiveValue(input, "sub_geography")
        updateSelectInput(
          inputId = "sub_geography",
          choices = unique(na.omit(geography_df()$SUB_GEOGRAPHY_NAME))
        )
      }
    )

    # Filter the data based on the sub geography
    sub_geography_df <- reactive({
      
      req(input$geography)
      req(input$sub_geography)
      
      geography_df() %>%
        dplyr::filter(SUB_GEOGRAPHY_NAME == input$sub_geography)
      
    })
    
    # Pull overall percentage of female patients
    percentage_female_patients <- reactive({
      
      req(input$geography)
      req(input$sub_geography)
      
      # Filter to overall period
      overall_df <- sub_geography_df() %>%
        dplyr::filter(YEAR_MONTH == "Overall")
      
      # Get the total female patients
      female_patients_df <- overall_df %>%
        dplyr::summarise(
          TOTAL_FEMALE_PATIENTS = 
            sum(ifelse(!is.na(GENDER) & GENDER == "Female", TOTAL_PATIENTS, 0)),
          TOTAL_PATIENTS = sum(TOTAL_PATIENTS)
        )
      
      # Calculate the percentage of patients
      female_patients_df <- female_patients_df %>%
        dplyr::mutate(
          PCT_FEMALE_PATIENTS = TOTAL_FEMALE_PATIENTS / TOTAL_PATIENTS * 100
        )
      
      # Apply SDC to percentage of female patients
      female_patients_df <- female_patients_df %>%
        dplyr::mutate(
          SDC = ifelse(TOTAL_FEMALE_PATIENTS %in% c(1, 2, 3, 4), 1, 0),
          SDC_PCT_FEMALE_PATIENTS =
            ifelse(
              test = SDC == 1, 
              yes = "c",
              no = as.character(janitor::round_half_up(PCT_FEMALE_PATIENTS))
            )
        )
      
      # Pull percentage
      female_patients_df %>%
        dplyr::pull(SDC_PCT_FEMALE_PATIENTS)
      
    })
    
    # Pull percentage of elderly female patients
    percentage_elderly_female_patients <- reactive({
      
      req(input$geography)
      req(input$sub_geography)
      
      # Filter to overall period
      overall_df <- sub_geography_df() %>%
        dplyr::filter(YEAR_MONTH == "Overall")
      
      # Get the total elderly female patients
      elderly_female_patients_df <- overall_df %>%
        dplyr::summarise(
          TOTAL_ELDERLY_FEMALE_PATIENTS = sum(
            ifelse(
              test = GENDER == "Female" & AGE_BAND %in% c("85-89", "90+"),
              yes = TOTAL_PATIENTS,
              no = 0
            )
          ),
          TOTAL_PATIENTS = sum(TOTAL_PATIENTS)
        )
      
      # Calculate the percentage of patients
      elderly_female_patients_df <- elderly_female_patients_df %>%
        dplyr::mutate(
          PCT_ELDERLY_FEMALE_PATIENTS = 
            TOTAL_ELDERLY_FEMALE_PATIENTS / TOTAL_PATIENTS * 100
        )
      
      # Apply SDC to percentage of elderly female patients
      elderly_female_patients_df <- elderly_female_patients_df %>%
        dplyr::mutate(
          SDC = ifelse(TOTAL_ELDERLY_FEMALE_PATIENTS %in% c(1, 2, 3, 4), 1, 0),
          SDC_PCT_ELDERLY_FEMALE_PATIENTS =
            ifelse(
              test = SDC == 1, 
              yes = "c", 
              no = as.character(
                janitor::round_half_up(PCT_ELDERLY_FEMALE_PATIENTS)
              )
            )
        )
      
      # Pull percentage
      elderly_female_patients_df %>%
        dplyr::pull(SDC_PCT_ELDERLY_FEMALE_PATIENTS)
      
    })
    
    # Pull average monthly patients
    average_monthly_patients <- reactive({
      
      req(input$geography)
      req(input$sub_geography)
      
      # Filter to non overall period
      non_overall_df <- sub_geography_df() %>%
        dplyr::filter(YEAR_MONTH == "Overall")
      
      # Get the average monthly patients
      average_monthly_patients_df <- non_overall_df %>%
        dplyr::summarise(AVERAGE_TOTAL_PATIENTS = sum(TOTAL_PATIENTS) / 12)
      
      # Apply SDC to average monthly patients
      average_monthly_patients_df <- average_monthly_patients_df %>%
        dplyr::mutate(
          SDC = ifelse(AVERAGE_TOTAL_PATIENTS %in% c(1, 2, 3, 4), 1, 0),
          SDC_AVERAGE_TOTAL_PATIENTS =
            ifelse(
              test = SDC == 1, 
              yes = "c", 
              no = as.character(round(AVERAGE_TOTAL_PATIENTS, -1))
            )
        )
      
      # Pull average
      average_monthly_patients_df %>%
        dplyr::pull(SDC_AVERAGE_TOTAL_PATIENTS)
      
    })
    
    # Create the reactive text to go inside the chart
    output$chart_text <- shiny::renderUI({
      
      req(input$geography)
      req(input$sub_geography)
      
      tagList(
        p(
          id = "medium",
          ifelse(input$sub_geography == "Overall", "", "In "),
          input$sub_geography, " we estimate", 
          tags$b(paste0(percentage_female_patients(), "%")), "of care home ",
          "patients are females and", 
          tags$b(paste0(percentage_elderly_female_patients(), "%")), "are ",
          "females aged 85 or over."
        ),
        p(
          id = "medium",
          "There are an estimated", tags$b(average_monthly_patients()), 
          "average number of monthly care home patients."
        )
      )
      
    })
    
    # Pull the metric we are interested in
    metric_df <- reactive ({
      
      req(input$geography)
      req(input$sub_geography)
      req(input$metric)
      
      sub_geography_df() %>%
        dplyr::select(
          dplyr::all_of(
            c(
              "YEAR_MONTH", 
              "GEOGRAPHY",
              "SUB_GEOGRAPHY_NAME",
              "SUB_GEOGRAPHY_CODE",
              "GENDER",
              "AGE_BAND",
              input$metric
            )
          )
        )
      
    })
    
    # Pull the NA gender patients
    patients_with_na_gender <- reactive({
      
      req(input$geography)
      req(input$sub_geography)
      req(input$metric)
      
      print(metric_df())
      
      metric_df() %>%
        dplyr::filter(YEAR_MONTH == "Overall" & is.na(GENDER)) %>%
        # Format number
        dplyr::mutate(
          "{input$metric}" := ifelse(
            test = is.na(.data[[input$metric]]), 
            yes = "c", 
            no = as.character(.data[[input$metric]])
          )
        ) %>%
        dplyr::pull(.data[[input$metric]])
      
    })
    
    # Filter out unknown genders for the plot
    plot_df <- reactive({
      
      req(input$geography)
      req(input$sub_geography)
      req(input$metric)
      
      metric_df() %>%
        dplyr::filter(!is.na(GENDER))
      
    })

    # Swap NAs for "c" for data download
    download_df <- reactive({

      req(input$geography)
      req(input$sub_geography)
      req(input$metric)
      
      plot_df() %>%
        dplyr::mutate(
          "{input$metric}" := ifelse(
            test = is.na(.data[[input$metric]]), 
            yes = "c", 
            no = as.character(.data[[input$metric]])
          )
        )
      
    })
    
    # Add a download button
    mod_download_server(
      id = "download_patients_by_geography_and_gender_and_age_band_chart",
      filename = "patients_by_geography_and_gender_and_age_band_chart.csv",
      export_data = download_df()
    )

    # Pull the max value
    max_value <- reactive({
      
      req(input$geography)
      req(input$sub_geography)
      req(input$metric)

      max(plot_df()[[input$metric]], na.rm = TRUE)
      
    })

    # Format for highcharter animation.
    plot_series_list <- reactive({
      
      req(input$geography)
      req(input$sub_geography)
      req(input$metric)
      
      plot_df() %>%
        # Negate male values so the butterfly chart works
        dplyr::mutate(
          value = 
            .data[[input$metric]] * ifelse(GENDER == "Male", 1, -1)
        ) %>%
        dplyr::group_by(AGE_BAND, GENDER) %>%
        dplyr::do(data = list(sequence = .$value)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(GENDER) %>%
        dplyr::do(data = .$data) %>%
        dplyr::mutate(name = GENDER) %>%
        highcharter::list_parse()
    })

    # Pyramid plot for age band and gender
    output$patients_by_gender_and_age_band_chart <-
      highcharter::renderHighchart({
        
        req(input$geography)
        req(input$geography)
        req(input$metric)

        # Create the base of the chart
        chart <- highcharter::highchart() %>%
          highcharter::hc_chart(type = "bar", marginBottom = 100) %>%
          highcharter::hc_add_series_list(x = plot_series_list()) %>%
          highcharter::hc_motion(
            labels = unique(plot_df()$YEAR_MONTH),
            series = c(0, 1)
          ) %>%
          theme_nhsbsa(palette = "gender") %>%
          highcharter::hc_caption(
            text = paste0(
              "This chart excludes ", patients_with_na_gender(), 
              ifelse(input$metric == "SDC_TOTAL_PATIENTS", " ", "% "),
              "patients with an unknown gender."
            ),
            margin = 5,
            align = "right"
          ) %>%
          highcharter::hc_xAxis(
            title = list(text = "Age Band"),
            categories = sort(unique(plot_df()$AGE_BAND)),
            reversed = FALSE
          )

        if (input$metric == "SDC_TOTAL_PATIENTS") {
          
          chart %>%
            highcharter::hc_yAxis(
              title = list(
                text = "Estimated number of care home patients (thousands)"
              ),
              min = -ceiling(max_value() / 5) * 5,
              max = ceiling(max_value() / 5) * 5,
              labels = list(
                formatter = htmlwidgets::JS(
                  "
                    function() {
                      return Math.abs(this.value) / 1000;
                    }
                  "
                )
              )
            ) %>%
            highcharter::hc_tooltip(
              shared = FALSE,
              useHTML = TRUE,
              formatter = htmlwidgets::JS(
                "
                function() {
    
                  if(Math.abs(this.point.y) >= 1000) {
    
                    outHTML =
                      '<b>Gender: </b>' + this.series.name + '<br>' +
                      '<b>Age band (5 years): </b>' + this.point.category + '<br/>' +
                      '<b>Number of patients: </b>' + Highcharts.numberFormat(Math.abs(this.point.y), 0);
    
                  } else {
    
                    outHTML =
                      '<b>Gender: </b>' + this.series.name + '<br>' +
                      '<b>Age band (5 years): </b>' + this.point.category + '<br/>' +
                      '<b>Number of patients: </b>' + Math.abs(this.point.y);
    
                  }
    
                  return(outHTML);
    
                }
                "
              )
            )
          
        } else {
          
          chart %>%
            highcharter::hc_yAxis(
              title = list(
                text = "Number of care home patients as percentage of all care home patients (%)"
              ),
              min = -ceiling(max_value() / 5) * 5,
              max = ceiling(max_value() / 5) * 5,
              labels = list(
                formatter = highcharter::JS(
                  "
                    function() {
                      return Math.abs(this.value);
                    }
                  "
                )
              )
            ) %>%
            highcharter::hc_tooltip(
              shared = FALSE,
              formatter = highcharter::JS(
                "
                  function () {
      
                    outHTML =
                      '<b>Gender: </b>' + this.series.name + '<br>' +
                      '<b>Age band (5 years): </b>' + this.point.category + '<br/>' +
                      '<b>Percentage: </b>' + Math.abs(this.point.y) + '%';
      
                    return outHTML;
                  }
                "
              )
            )
          
        }
        
      })

    # Add IMD chart
    output$imd_quintile_chart <- highcharter::renderHighchart({

      # highcharter plot
      careHomePrescribingScrollytellR::index_of_multiple_deprivation_df %>%
        highcharter::hchart(
          type = "column",
          highcharter::hcaes(x = IMD_QUINTILE, y = PROP),
          stacking = "normal"
        ) %>%
        theme_nhsbsa() %>%
        highcharter::hc_legend(enabled = FALSE) %>%
        highcharter::hc_xAxis(
          categories = c(
            "1<br>Most<br>deprived", 2:4, "5<br>Least<br>deprived"
          ),
          title = list(text = "Deprivation quintile")
        ) %>%
        highcharter::hc_yAxis(
          title = list(text = "% of care home patients")
        ) %>%
        highcharter::hc_tooltip(
          shared = FALSE,
          formatter = highcharter::JS(
            "
            function () { 
              return '<b>Quintile: </b>' + parseInt(this.point.category) + ' (' + this.point.y + '%)'} 
            "
          )
        )
    })

  })
}

## To be copied in the UI
# mod_02_demographics_ui("02_demographics_ui_1")

## To be copied in the server
# mod_02_demographics_server("02_demographics_ui_1")
