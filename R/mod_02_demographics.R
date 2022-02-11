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
    h2(
      "Demographic estimates for",
      tippy(
        text = "older care home patients",
        tooltip = tooltip_text$care_home
      ),
      "receiving prescriptions"
    ),
    p(tags$b("The older care home population fluctuates")),
    p(
      "We estimate", tags$b("472 thousand patients"), "aged 65+ years ",
      "received at least one prescription item in a care home during 2020/21 ",
      "and an average of", tags$b("286 thousand"), "in any given month. This ",
      "difference in numbers is explained by two key factors:"
    ),
    tags$ul(
      tags$li(
        "The population is not stable – some patients become 65 years during ",
        "the year, some move in or out of the care home and others may die."
      ),
      tags$li(
        "Not all care home patients receive a prescription in every month ",
        "they are in a care home - we estimate around 7 in 10 do."
      )
    ),
    p(
      "For this reason, when we calculate per patient estimates, we use the ",
      "monthly average number of care home patients."
    ),
    p(
      "The chart shows the prescribing status of each of these 472 thousand ",
      "patients by month during 2020/21. Around 3 in 10 (31%) received ",
      "prescriptions in a care home in all 12 months."
    ),
    nhs_card(
      heading = "Monthly prescribing status of patients aged 65+ in England who received at least one prescription item in a care home (2020/21)",
      highcharter::highchartOutput(
        outputId = ns("patients_by_prescribing_status_chart"),
        height = "350px"
      ),
      mod_nhs_download_ui(
        id = ns("download_patients_by_prescribing_status_chart")
      )
    ),
    br(),
    p(
      tags$b(
        "We estimate two thirds of older care home patients are female and 6 ",
        "in 10 are aged 85+ years"
      )
    ),
    p(
      "Our estimated monthly average of",
      tags$b("286 thousand older care home patients"), "receiving ",
      "prescriptions, represents around", tags$b("5% of patients age 65+"),
      "years receiving prescription items each month."
    ),
    p(
      "The age and gender profile is broadly comparable to",
      enurl(
        text = "ONS Estimates of care home patients from April 2020.",
        url = "https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/12215carehomeandnoncarehomepopulationsusedinthedeathsinvolvingcovid19inthecaresectorarticleenglandandwales"
      ),
      "The profile is shown in the chart below at an overall level and can be ",
      "explored by region, local authority and STP (Sustainability and ",
      "Transformation Plan area)."
    ),
    nhs_card(
      heading = "Age band and gender of estimated older care home patients in England (2020/21)",
      nhs_grid_2_col(
        nhs_selectInput(
          inputId = ns("geography"),
          label = "Geography",
          choices = names(careHomePrescribingScrollytellR::geographys),
          full_width = TRUE
        ),
        nhs_selectInput(
          inputId = ns("sub_geography"),
          label = "Sub Geography",
          choices = NULL, # dynamically generated
          full_width = TRUE
        )
      ),
      highcharter::highchartOutput(
        outputId = ns("patients_by_geography_and_gender_and_age_band_chart"),
        height = "350px"
      ),
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt;",
        "This excludes << 1% of patients with an unknown gender and where the number of patients is less than 5 the data has been redacted."
      ),
      mod_nhs_download_ui(
        id = ns("download_patients_by_geography_and_gender_and_age_band_chart")
      )
    ),
    br(),
    p(
      tags$b(
        "Similar proportions of older care home patients live in ",
        tippy(
          text = "residential homes",
          tooltip = tooltip_text$residential_home
        ),
        " and ",
        tippy(
          text = "nursing homes",
          tooltip = tooltip_text$nursing_home
        )
      )
    ),
    p(
      "We estimate similar proportions of care home patients aged 65+ living ",
      "in ",
      tippy(
        text = "residential homes",
        tooltip = tooltip_text$residential_home
      ),
      " (41%) and ",
      tippy(
        text = "nursing homes",
        tooltip = tooltip_text$nursing_home
      ),
      " (45%) on average each month.", "A small percentage (3%) appear in ",
      "both settings and there are 11% who we were unable to link to the ",
      enurl(
        text = "CQC dataset",
        url = "https://anypoint.mulesoft.com/exchange/portals/care-quality-commission-5/4d36bd23-127d-4acf-8903-ba292ea615d4/cqc-syndication-1/"
      ),
      " due to incomplete address information in order to attribute this flag."
    ),
    p(
      tags$b(
        "There is little variation in numbers of older care home patients by ",
        "deprivation"
      )
    ),
    p(
      "Care home patient's prescriptions were allocated an ",
      enurl(
        text = "Index of Multiple Deprivation (IMD)",
        url = "https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019"
      ),
      " rank and associated quintile based on the area in which the care home ",
      "is located. On average, the proportion is very close to 20% in each ",
      tags$b("IMD quintile,"),
      " which suggests equal distribution and little variation."
    ),
    nhs_card(
      heading = "Deprivation quintile of older care home patients in England (2020/21)",
      highcharter::highchartOutput(
        outputId = ns("patients_by_imd_chart"),
        height = "250px"
      ),
      mod_nhs_download_ui(
        id = ns("download_patients_by_imd_chart")
      )
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
    mod_nhs_download_server(
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
          highcharter::hc_yAxis(title = list(text = "Number of patients")) %>%
          highcharter::hc_tooltip(
            shared = TRUE,
            headerFormat = "<b> {point.value} </b>"
          )
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

    # Pull the max value
    max_value <- reactive({
      req(input$geography)
      req(input$sub_geography)

      patients_by_geography_and_gender_and_age_band_sub_geography_df() %>%
        dplyr::summarise(max(SDC_TOTAL_PATIENTS, na.rm = TRUE)) %>%
        dplyr::pull()
    })

    # Pull the total
    total <- reactive({
      req(input$geography)
      req(input$sub_geography)

      patients_by_geography_and_gender_and_age_band_sub_geography_df() %>%
        dplyr::summarise(TOTAL_PATIENTS = sum(TOTAL_PATIENTS, na.rm = TRUE)) %>%
        dplyr::mutate(
          SDC_TOTAL_PATIENTS = ifelse(
            test = TOTAL_PATIENTS %in% c(1, 2, 3, 4),
            yes = "c",
            no = format(round(TOTAL_PATIENTS, -1), big.mark = ",")
          )
        ) %>%
        dplyr::pull(SDC_TOTAL_PATIENTS)
    })

    # Pull percentage of female patients
    percentage_female_patients <- reactive({
      req(input$geography)
      req(input$sub_geography)

      # Get the total female patients
      female_patients_df <-
        patients_by_geography_and_gender_and_age_band_sub_geography_df() %>%
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
              no = as.character(janitor::round_half_up(PCT_FEMALE_PATIENTS, 1))
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

      # Get the total elderly female patients
      elderly_female_patients_df <-
        patients_by_geography_and_gender_and_age_band_sub_geography_df() %>%
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
                janitor::round_half_up(PCT_ELDERLY_FEMALE_PATIENTS, 1)
              )
            )
        )

      # Pull percentage
      elderly_female_patients_df %>%
        dplyr::pull(SDC_PCT_ELDERLY_FEMALE_PATIENTS)
    })

    # Pull the number of NA gender patients
    patients_with_na_gender <- reactive({
      req(input$geography)
      req(input$sub_geography)

      patients_by_geography_and_gender_and_age_band_sub_geography_df() %>%
        dplyr::filter(is.na(GENDER)) %>%
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

    # Swap NAs for "c" for data download and subset columns
    patients_by_geography_and_gender_and_age_band_download_df <- reactive({
      req(input$geography)
      req(input$sub_geography)

      patients_by_geography_and_gender_and_age_band_sub_geography_df() %>%
        dplyr::mutate(
          SDC_TOTAL_PATIENTS = ifelse(
            test = is.na(SDC_TOTAL_PATIENTS),
            yes = "c",
            no = as.character(SDC_TOTAL_PATIENTS)
          ),
          SDC_PCT_PATIENTS = ifelse(
            test = is.na(SDC_PCT_PATIENTS),
            yes = "c",
            no = as.character(SDC_PCT_PATIENTS)
          )
        ) %>%
        dplyr::select(-c(TOTAL_PATIENTS, PCT_PATIENTS))
    })

    # Filter out unknown genders for the plot and format
    patients_by_geography_and_gender_and_age_band_plot_df <- reactive({
      req(input$geography)
      req(input$sub_geography)

      patients_by_geography_and_gender_and_age_band_sub_geography_df() %>%
        dplyr::filter(!is.na(GENDER)) %>%
        # Negate male values so the butterfly chart works
        dplyr::mutate(
          SDC_TOTAL_PATIENTS =
            SDC_TOTAL_PATIENTS * ifelse(GENDER == "Male", 1, -1),
          SDC_PCT_PATIENTS =
            SDC_PCT_PATIENTS * ifelse(GENDER == "Male", 1, -1)
        )
    })

    # Add a download button
    mod_nhs_download_server(
      id = "download_patients_by_geography_and_gender_and_age_band_chart",
      filename = "patients_by_geography_and_gender_and_age_band_chart.csv",
      export_data = patients_by_geography_and_gender_and_age_band_download_df
    )

    # Pyramid plot for age band and gender
    output$patients_by_geography_and_gender_and_age_band_chart <-
      highcharter::renderHighchart({
        req(input$geography)
        req(input$sub_geography)

        # Process annotation
        text <- paste(
          ifelse(input$sub_geography == "Overall", "", "In"),
          input$sub_geography, "there are an estimated", tags$b(total()),
          "care home patients in 2020/21, of which",
          tags$b(paste0(percentage_female_patients(), "%")), "are females and",
          tags$b(paste0(percentage_elderly_female_patients(), "%")), "were",
          "females aged 85 or over."
        )

        # Create the chart
        patients_by_geography_and_gender_and_age_band_plot_df() %>%
          highcharter::hchart(
            type = "bar",
            highcharter::hcaes(
              x = AGE_BAND,
              y = SDC_TOTAL_PATIENTS,
              group = GENDER
            )
          ) %>%
          theme_nhsbsa(palette = "gender") %>%
          highcharter::hc_annotations(
            list(
              labels = list(
                list(
                  point = list(
                    x = 0,
                    # Need -1 otherwise it fails when max_value() is axis max
                    y = max_value() - 1,
                    xAxis = 0,
                    yAxis = 0
                  ),
                  text = text,
                  style = list(
                    width = 150,
                    fontSize = "9pt"
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
            title = list(text = "Age band"),
            categories =
              patients_by_geography_and_gender_and_age_band_plot_df()$AGE_BAND %>%
                unique() %>%
                sort(),
            reversed = FALSE
          ) %>%
          highcharter::hc_yAxis(
            title = list(text = "Number of patients"),
            min = -max_value(),
            max = max_value(),
            labels = list(
              formatter = highcharter::JS(
                "
                function() {

                  outHTML = this.axis.defaultLabelFormatter.call(this)

                  return outHTML.replace('-', '')

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

                outHTML =
                  '<b>Gender: </b>' + this.series.name + '<br>' +
                  '<b>Age band: </b>' + this.point.category + '<br/>' +
                  '<b>Number of patients: </b>' + Highcharts.numberFormat(Math.abs(this.point.y), 0) + '<br>' +
                  '<b>Percentage of patients: </b>' + Highcharts.numberFormat(Math.abs(this.point.SDC_PCT_PATIENTS), 1) + '%'

                return outHTML

              }
              "
            )
          )
      })

    # Patients by IMD chart

    # Swap NAs for "c" for data download
    patients_by_imd_download_df <-
      careHomePrescribingScrollytellR::patients_by_imd_df %>%
      dplyr::mutate(
        SDC_TOTAL_PATIENTS = ifelse(
          test = is.na(SDC_TOTAL_PATIENTS),
          yes = "c",
          no = as.character(SDC_TOTAL_PATIENTS)
        ),
        SDC_PCT_PATIENTS = ifelse(
          test = is.na(SDC_PCT_PATIENTS),
          yes = "c",
          no = as.character(SDC_PCT_PATIENTS)
        )
      )

    # Add a download button
    mod_nhs_download_server(
      id = "download_patients_by_imd_chart",
      filename = "patients_by_imd_chart.csv",
      export_data = patients_by_imd_download_df
    )

    # Add IMD chart
    output$patients_by_imd_chart <- highcharter::renderHighchart({

      # highcharter plot
      careHomePrescribingScrollytellR::patients_by_imd_df %>%
        highcharter::hchart(
          type = "column",
          highcharter::hcaes(
            x = IMD_QUINTILE,
            y = SDC_TOTAL_PATIENTS
          ),
          stacking = "normal"
        ) %>%
        theme_nhsbsa() %>%
        highcharter::hc_legend(enabled = FALSE) %>%
        highcharter::hc_xAxis(
          categories = c(
            NA, "1<br>Most<br>deprived", 2:4, "5<br>Least<br>deprived"
          ),
          title = list(text = "Deprivation quintile")
        ) %>%
        highcharter::hc_yAxis(
          title = list(
            text = "Number of patients"
          )
        ) %>%
        highcharter::hc_tooltip(
          shared = FALSE,
          formatter = highcharter::JS(
            "
            function () {

              outHTML =
                '<b>Quintile: </b>' + parseInt(this.point.category) + '<br>' +
                '<b>Number of patients: </b>' + Highcharts.numberFormat(this.point.y, 0) + '<br>' +
                '<b>Percentage of patients: </b>' + this.point.SDC_PCT_PATIENTS + '%'

              return outHTML

            }
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
