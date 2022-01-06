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
    h6("The older care home population fluctuates"),
    p(
      "We estimate 460 thousand patients aged 65+ years received at least one ",
      "prescription item in a care home during 2020/21 and an average of 285 ",
      "thousand in any given month. This difference in numbers is explained ",
      "by two key factors:"
    ),
    tags$ul(
      tags$li(
        style = "font-size: 16pt;",
        "The population is not stable – some patients become 65 years during ",
        "the year, some move in or out of the care home and others may die."
      ),
      tags$li(
        style = "font-size: 16pt;",
        "Not all care home patients receive a prescription in every month ",
        "they are in a care home - we estimate around 7 in 10 do."
      )
    ),
    p(
      "For this reason, when we calculate per resident estimates, we use the ",
      "monthly average number of care home patients."
    ),
    p(
      "The chart shows the prescribing status of each of these 460 thousand ",
      "patients by month during 2020/21."
    ),
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;",
      h6(
        "Monthly prescribing status of patients aged 65+ who recieved at ",
        "least one prescription item in a care home during 2020/21"
      ),
      highcharter::highchartOutput(
        outputId = ns("patients_by_prescribing_status_chart"),
        height = "350px",
        width = "900px"
      )
    ),
    mod_download_ui(
      id = ns("download_patients_by_prescribing_status_chart")
    ),
    br(),
    h6(
      "We estimate two thirds of older care home patients are female and 6 in ",
      "10 are aged 85+ years"
    ),
    p(
      "Overall, we estimate a monthly average of ", 
      tags$b("285 thousand care home patients,"), " aged 65+ years receiving ",
      "prescriptions, which accounts for around 4% of patients aged 65+ years ",
      "receiving prescription items each month."
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
        style = "margin-bottom: 0;",
        "Age band and gender of estimated older care home patients in England ",
        "(2020/21)"
      ),
      col_6(
        style = "margin-bottom: 0;",
        div(
          selectInput(
            inputId = ns("geography"),
            label = "Geography",
            choices = names(careHomePrescribingScrollytellR::geographys),
            width = "100%"
          )
        )
      ),
      col_6(
        style = "margin-bottom: 0;",
        div(
          selectInput(
            inputId = ns("sub_geography"),
            label = "Sub Geography",
            choices = NULL, # dynamically generated
            width = "100%"
          )
        )
      ),
      col_12(
        style = "margin-bottom: 0;",
        radioButtons(
          inputId = ns("patients_by_geography_and_gender_and_age_band_metric"),
          label = NULL,
          choices = c(
            "Count" = "SDC_TOTAL_PATIENTS",
            "Percentage" = "SDC_PCT_PATIENTS"
          ),
          inline = TRUE,
          width = "100%"
        )
      ),
      col_8(
        highcharter::highchartOutput(
          outputId = ns("patients_by_geography_and_gender_and_age_band_chart"),
          height = "400px",
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
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;",
      h6(
        style = "margin-bottom: 0;",
        "Deprivation quintile of older care home patients in England (2020/21)"
      ),
      radioButtons(
        inputId = ns("patients_by_imd_metric"),
        label = NULL,
        choices = c(
          "Count" = "SDC_TOTAL_PATIENTS",
          "Percentage" = "SDC_PCT_PATIENTS"
        ),
        inline = TRUE,
        width = "100%"
      ),
      highcharter::highchartOutput(
        outputId = ns("patients_by_imd_chart"),
        height = "300px",
        width = "900px"
      )
    ),
    mod_download_ui(
      id = ns("download_patients_by_imd_chart")
    )
  )
}

#' 02_demographics Server Functions
#'
#' @noRd
mod_02_demographics_server <- function(id, export_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Patients by prescribing status chart
    
    # Swap NAs for "c" for data download
    patients_by_prescribing_status_df <- reactive({

      careHomePrescribingScrollytellR::patients_by_prescribing_status_df %>%
        dplyr::mutate(
          SDC_TOTAL_PATIENTS := ifelse(
            test = is.na(SDC_TOTAL_PATIENTS),
            yes = "c",
            no = as.character(SDC_TOTAL_PATIENTS)
          )
        )
    })
    
    # Add a download button
    mod_download_server(
      id = "download_patients_by_prescribing_status_chart",
      filename = "patients_by_prescribing_status_chart.csv",
      export_data = patients_by_prescribing_status_df()
    )
    
    # Create chart
    output$patients_by_prescribing_status_chart <- 
      highcharter::renderHighchart({
      
        careHomePrescribingScrollytellR::patients_by_prescribing_status_df %>%
          dplyr::mutate(YEAR_MONTH = as.character(YEAR_MONTH)) %>%
          highcharter::hchart(
            type = "column", 
            highcharter::hcaes(
              x = YEAR_MONTH, 
              y = SDC_TOTAL_PATIENTS, 
              group = PRESCRIBING_STATUS
            ), 
            stacking = "normal"
          ) %>%
          theme_nhsbsa() %>%
          highcharter::hc_xAxis(title = list(text = "Year Month")) %>%
          highcharter::hc_yAxis(title = list(text = "Total patients"))
        
      })
    
    # Patients by geography and gender and age band chart
    
    # Filter to relevant data for this chart
    patients_by_geography_and_gender_and_age_band_df <-
      careHomePrescribingScrollytellR::patients_by_geography_and_gender_and_age_band_df %>%
      dplyr::filter(
        dplyr::across(c(GEOGRAPHY, SUB_GEOGRAPHY_NAME, GENDER), not_na)
      )

    # Handy resource: https://mastering-shiny.org/action-dynamic.html

    # Filter the data based on the geography
    patients_by_geography_and_gender_and_age_band_geography_df <- reactive({
      req(input$geography)

      careHomePrescribingScrollytellR::patients_by_geography_and_gender_and_age_band_df %>%
        dplyr::filter(GEOGRAPHY == input$geography)
    })

    # Pull the number of NA sub geography patients
    patients_in_na_sub_geography <- reactive({
      req(input$geography)

      patients_by_geography_and_gender_and_age_band_geography_df() %>%
        dplyr::filter(
          is.na(SUB_GEOGRAPHY_NAME),
          YEAR_MONTH == "Overall",
          # We forced these to have NA gender and age band in data-raw/
          is.na(GENDER),
          is.na(AGE_BAND)
        ) %>%
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
      eventExpr = patients_by_geography_and_gender_and_age_band_geography_df(),
      handlerExpr = {
        freezeReactiveValue(input, "sub_geography")
        updateSelectInput(
          inputId = "sub_geography",
          choices =
            patients_by_geography_and_gender_and_age_band_geography_df()$SUB_GEOGRAPHY_NAME %>%
              na.omit() %>%
              unique()
        )
      }
    )

    # Filter the data based on the sub geography
    patients_by_geography_and_gender_and_age_band_sub_geography_df <- reactive({
      req(input$geography)
      req(input$sub_geography)

      patients_by_geography_and_gender_and_age_band_geography_df() %>%
        dplyr::filter(SUB_GEOGRAPHY_NAME == input$sub_geography)
    })

    # Pull overall percentage of female patients
    percentage_female_patients <- reactive({
      req(input$geography)
      req(input$sub_geography)

      # Filter to overall period
      overall_df <-
        patients_by_geography_and_gender_and_age_band_sub_geography_df() %>%
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
      overall_df <-
        patients_by_geography_and_gender_and_age_band_sub_geography_df() %>%
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
      non_overall_df <-
        patients_by_geography_and_gender_and_age_band_sub_geography_df() %>%
        dplyr::filter(YEAR_MONTH != "Overall")

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

    # Pull the metric we are interested in
    patients_by_geography_and_gender_and_age_band_metric_df <- reactive({
      req(input$geography)
      req(input$sub_geography)
      req(input$patients_by_geography_and_gender_and_age_band_metric)

      patients_by_geography_and_gender_and_age_band_sub_geography_df() %>%
        dplyr::select(
          dplyr::all_of(
            c(
              "YEAR_MONTH",
              "GEOGRAPHY",
              "SUB_GEOGRAPHY_NAME",
              "SUB_GEOGRAPHY_CODE",
              "GENDER",
              "AGE_BAND",
              input$patients_by_geography_and_gender_and_age_band_metric
            )
          )
        )
    })

    # Pull the NA gender patients
    patients_with_na_gender <- reactive({
      req(input$geography)
      req(input$sub_geography)
      req(input$patients_by_geography_and_gender_and_age_band_metric)

      patients_by_geography_and_gender_and_age_band_metric_df() %>%
        dplyr::filter(YEAR_MONTH == "Overall" & is.na(GENDER)) %>%
        # Format number
        dplyr::mutate(
          "{input$patients_by_geography_and_gender_and_age_band_metric}" := ifelse(
            test = is.na(.data[[input$patients_by_geography_and_gender_and_age_band_metric]]),
            yes = "c",
            no = as.character(.data[[input$patients_by_geography_and_gender_and_age_band_metric]])
          )
        ) %>%
        dplyr::pull(.data[[input$patients_by_geography_and_gender_and_age_band_metric]])
    })

    # Filter out unknown genders for the plot
    patients_by_geography_and_gender_and_age_band_plot_df <- reactive({
      req(input$geography)
      req(input$sub_geography)
      req(input$patients_by_geography_and_gender_and_age_band_metric)

      patients_by_geography_and_gender_and_age_band_metric_df() %>%
        dplyr::filter(!is.na(GENDER))
    })

    # Swap NAs for "c" for data download
    patients_by_geography_and_gender_and_age_band_download_df <- reactive({
      req(input$geography)
      req(input$sub_geography)
      req(input$patients_by_geography_and_gender_and_age_band_metric)

      patients_by_geography_and_gender_and_age_band_plot_df() %>%
        dplyr::mutate(
          "{input$patients_by_geography_and_gender_and_age_band_metric}" := ifelse(
            test = is.na(.data[[input$patients_by_geography_and_gender_and_age_band_metric]]),
            yes = "c",
            no = as.character(.data[[input$patients_by_geography_and_gender_and_age_band_metric]])
          )
        )
    })

    # Add a download button
    mod_download_server(
      id = "download_patients_by_geography_and_gender_and_age_band_chart",
      filename = "patients_by_geography_and_gender_and_age_band_chart.csv",
      export_data = patients_by_geography_and_gender_and_age_band_download_df()
    )

    # Pull the max value
    max_value <- reactive({
      req(input$geography)
      req(input$sub_geography)
      req(input$patients_by_geography_and_gender_and_age_band_metric)
      
      max(
        patients_by_geography_and_gender_and_age_band_plot_df()[[
        input$patients_by_geography_and_gender_and_age_band_metric
        ]],
        na.rm = TRUE
      )

    })

    # Format for highcharter animation.
    patients_by_geography_and_gender_and_age_band_plot_series_list <- reactive({
      req(input$geography)
      req(input$sub_geography)
      req(input$patients_by_geography_and_gender_and_age_band_metric)

      patients_by_geography_and_gender_and_age_band_plot_df() %>%
        # Negate male values so the butterfly chart works
        dplyr::mutate(
          value =
            .data[[input$patients_by_geography_and_gender_and_age_band_metric]] *
              ifelse(GENDER == "Male", 1, -1)
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
    output$patients_by_geography_and_gender_and_age_band_chart <-
      highcharter::renderHighchart({
        
        req(input$geography)
        req(input$sub_geography)
        req(input$patients_by_geography_and_gender_and_age_band_metric)
        
        # Process annotations
        text <- paste(
          ifelse(input$sub_geography == "Overall", "", "In"),
          input$sub_geography, "we estimate",
          tags$b(paste0(percentage_female_patients(), "%")), "of care home",
          "patients are females and",
          tags$b(paste0(percentage_elderly_female_patients(), "%")), "are",
          "females aged 85 or over.<br><br>There are an estimated",
          tags$b(prettyNum(average_monthly_patients(), big.mark = ",", scientific = FALSE)),
          "average number of monthly care home patients."
        )

        # Create the chart
        chart <- highcharter::highchart() %>%
          highcharter::hc_chart(type = "bar", marginBottom = 100) %>%
          highcharter::hc_add_series_list(
            x = patients_by_geography_and_gender_and_age_band_plot_series_list()
          ) %>%
          highcharter::hc_motion(
            labels =
              patients_by_geography_and_gender_and_age_band_plot_df()$YEAR_MONTH %>%
                unique(),
            series = c(0, 1)
          ) %>%
          theme_nhsbsa(palette = "gender") %>%
          highcharter::hc_caption(
            text = paste0(
              "This chart excludes ",
              switch(input$sub_geography,
                "Overall" = "",
                paste(
                  patients_in_na_sub_geography(), "patients with an unknown ",
                  "sub geography.<br/>Of the patients in the sub geography it ",
                  "also excludes "
                )
              ),
              patients_with_na_gender(),
              switch(input$patients_by_geography_and_gender_and_age_band_metric,
                "SDC_TOTAL_PATIENTS" = " ",
                "SDC_PCT_PATIENTS" = "% "
              ),
              "patients with an unknown gender."
            ),
            align = "right"
          ) %>%
          highcharter::hc_annotations(
            list(
              labels = list(
                list(
                  point = list(
                    x = - 0.5, 
                    # Need -1 otherwise it fails when max_value() is axis max
                    y = max_value() - 1,
                    xAxis = 0, 
                    yAxis = 0
                  ), 
                  text = text,
                  style = list(
                    width = 200, 
                    fontSize = "10pt"
                  )
                )
              ),
              labelOptions = list(
                backgroundColor = "#FFFFFF",
                borderWidth = 0,
                align = "right",
                verticalAlign = "top",
                useHTML = TRUE
              )
            )
          ) %>%
          highcharter::hc_xAxis(
            title = list(text = "Age Band"),
            categories =
              patients_by_geography_and_gender_and_age_band_plot_df()$AGE_BAND %>%
                unique() %>%
                sort(),
            reversed = FALSE
          ) %>%
          highcharter::hc_yAxis(
            title = list(
              text = paste(
                "Estimated number of care home patients",
                switch(input$patients_by_geography_and_gender_and_age_band_metric,
                  "SDC_TOTAL_PATIENTS" = "(thousands)",
                  "SDC_PCT_PATIENTS" = "as percentage of all care home patients (%)"
                )
              )
            ),
            min = -max_value(),
            max = max_value(),
            labels = list(
              formatter = highcharter::JS(
                paste(
                  "
                  function() {

                    outHTML = Math.abs(this.value)",
                  switch(input$patients_by_geography_and_gender_and_age_band_metric,
                    "SDC_TOTAL_PATIENTS" = " / 1000",
                    "SDC_PCT_PATIENTS" = ""
                  ),
                  "
                    return outHTML

                  }
                  "
                )
              )
            )
          ) %>%
          highcharter::hc_tooltip(
            shared = FALSE,
            useHTML = TRUE,
            formatter = htmlwidgets::JS(
              paste0(
                "
                function() {

                  outHTML =
                    '<b>Gender: </b>' + this.series.name + '<br>' +
                    '<b>Age band (5 years): </b>' + this.point.category + '<br/>' +
                    ",
                switch(input$patients_by_geography_and_gender_and_age_band_metric,
                  "SDC_TOTAL_PATIENTS" = "'<b>Number of patients: </b>' + Highcharts.numberFormat(Math.abs(this.point.y), 0)",
                  "SDC_PCT_PATIENTS" = "'<b>Percentage of patients: </b>' + Math.abs(this.point.y) + '%'"
                ),
                "
                  return outHTML

                }
                "
              )
            )
          )
      })

    # Patients by IMD chart

    # Pull the metric we are interested in
    patients_by_imd_metric_df <- reactive({
      req(input$patients_by_imd_metric)

      careHomePrescribingScrollytellR::patients_by_imd_df %>%
        dplyr::select(
          dplyr::all_of(c("IMD_QUINTILE", input$patients_by_imd_metric))
        )
    })

    # Pull the NA IMD patients
    patients_in_na_imd <- reactive({
      req(input$patients_by_imd_metric)

      patients_by_imd_metric_df() %>%
        dplyr::filter(is.na(IMD_QUINTILE)) %>%
        # Format number
        dplyr::mutate(
          "{input$patients_by_imd_metric}" := ifelse(
            test = is.na(.data[[input$patients_by_imd_metric]]),
            yes = "c",
            no = as.character(.data[[input$patients_by_imd_metric]])
          )
        ) %>%
        dplyr::pull(.data[[input$patients_by_imd_metric]])
    })

    # Filter out unknown IMDs for the plot
    patients_by_imd_plot_df <- reactive({
      req(input$patients_by_imd_metric)

      patients_by_imd_metric_df() %>%
        dplyr::filter(!is.na(IMD_QUINTILE))
    })

    # Swap NAs for "c" for data download
    patients_by_imd_download_df <- reactive({
      req(input$patients_by_imd_metric)

      patients_by_imd_plot_df() %>%
        dplyr::mutate(
          "{input$patients_by_imd_metric}" := ifelse(
            test = is.na(.data[[input$patients_by_imd_metric]]),
            yes = "c",
            no = as.character(.data[[input$patients_by_imd_metric]])
          )
        )
    })

    # Add a download button
    mod_download_server(
      id = "download_patients_by_imd_chart",
      filename = "patients_by_imd_chart.csv",
      export_data = patients_by_imd_download_df()
    )

    # Add IMD chart
    output$patients_by_imd_chart <- highcharter::renderHighchart({
      
      req(input$patients_by_imd_metric)

      # highcharter plot
      patients_by_imd_plot_df() %>%
        highcharter::hchart(
          type = "column",
          highcharter::hcaes(
            x = IMD_QUINTILE,
            y = .data[[input$patients_by_imd_metric]]
          ),
          stacking = "normal"
        ) %>%
        theme_nhsbsa() %>%
        highcharter::hc_caption(
          text = paste0(
            "This chart excludes ", patients_in_na_imd(),
            ifelse(
              test = input$patients_by_imd_metric == "SDC_TOTAL_PATIENTS",
              yes = " ",
              no = "% "
            ),
            "patients with an unknown IMD quintile."
          ),
          margin = 5,
          align = "right"
        ) %>%
        highcharter::hc_legend(enabled = FALSE) %>%
        highcharter::hc_xAxis(
          categories = c(
            NA, "1<br>Most<br>deprived", 2:4, "5<br>Least<br>deprived"
          ),
          title = list(text = "Deprivation quintile")
        ) %>%
        highcharter::hc_yAxis(
          title = list(
            text = paste(
              switch(input$patients_by_imd_metric,
                "SDC_TOTAL_PATIENTS" = "Number",
                "SDC_PCT_PATIENTS" = "Percentage"
              ),
              "of care home patients"
            )
          )
        ) %>%
        highcharter::hc_tooltip(
          shared = FALSE,
          formatter = highcharter::JS(
            paste0(
              "
              function () {

                outHTML =
                '<b>Quintile: </b>' + parseInt(this.point.category) + '<br>' + ",
              switch(input$patients_by_imd_metric,
                "SDC_TOTAL_PATIENTS" = "'<b>Total patients: </b>' + Highcharts.numberFormat(this.point.y, 0)",
                "SDC_PCT_PATIENTS" = "'<b>Percentage of patients: </b>' + this.point.y + '%'"
              ),
              "
                return outHTML

              }
              "
            )
          )
        )
    })
  })
}

## To be copied in the UI
# mod_02_demographics_ui("02_demographics_ui_1")

## To be copied in the server
# mod_02_demographics_server("02_demographics_ui_1")
