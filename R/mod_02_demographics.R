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
        inputId = ns("count_or_percentage"),
        label = "",
        choices = c("Count", "Percentage"),
        inline = TRUE,
        width = "100%"
      ),
      col_8(
        highcharter::highchartOutput(
          outputId = ns("demographics"),
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
          ns("text")
        )
      )
    ),
    downloadButton(outputId = ns("downloadData"), "Download Data"),
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

    # comma separate setting
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)


    # Filter to relevant data for this chart
    patients_by_geography_and_gender_and_age_band_df <-
      careHomePrescribingScrollytellR::patients_by_geography_and_gender_and_age_band_df %>%
      dplyr::filter(
        dplyr::across(c(GEOGRAPHY, SUB_GEOGRAPHY_NAME, GENDER), not_na)
      )

    # Handy resource: https://mastering-shiny.org/action-dynamic.html

    # Filter the data based on the geography
    geography_df <- reactive({
      req(input$geography)

      patients_by_geography_and_gender_and_age_band_df %>%
        dplyr::filter(GEOGRAPHY == input$geography)
    })

    # Update the list of choices for sub geography from the rows in the geography
    # dataframe
    observeEvent(
      eventExpr = geography_df(),
      handlerExpr = {
        freezeReactiveValue(input, "sub_geography")
        updateSelectInput(
          inputId = "sub_geography",
          choices = unique(geography_df()$SUB_GEOGRAPHY_NAME)
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

    # Pull % of females (and 85+) in sub geography
    female_ps <- reactive({
      req(input$geography)
      req(input$sub_geography)

      # Filter to overall period
      overall_df <- sub_geography_df() %>%
        dplyr::filter(YEAR_MONTH == "Overall")

      # Get the total patients
      total_patients <- sum(overall_df$TOTAL_PATIENTS)

      # Filter to female patients
      female_df <- overall_df %>%
        dplyr::filter(GENDER == "Female")

      # Get the total female patients
      female_patients <- sum(female_df$TOTAL_PATIENTS)

      # Filter to 85+ patients
      # Female only
      female_85_plus_df <- overall_df %>%
        dplyr::filter(GENDER == "Female" & AGE_BAND %in% c("85-89", "90+"))

      # Get the total female 85+ patients
      female_85_plus_patients <- sum(female_85_plus_df$TOTAL_PATIENTS)

      # Calculate the proportions
      p <- c(female_patients, female_85_plus_patients) / total_patients

      paste0(janitor::round_half_up(p * 100), "%")
    })

    # Pull monthly average care home patients
    # same here, we use the raw number and round at the end
    average_monthly_patients <- reactive({
      req(input$geography)
      req(input$sub_geography)

      # Remove the overall category as this is the sum over the year
      non_overall_df <- sub_geography_df() %>%
        dplyr::filter(YEAR_MONTH != "Overall")

      # Output the mean
      # This one will bring average care home by gender and month. we are interested in month

      monthly_average <- non_overall_df %>%
        dplyr::summarise(monthly_average = sum(TOTAL_PATIENTS) / 12) %>%
        dplyr::ungroup()


      # change to the nearest 10
      monthly_average <- round(monthly_average, -1)

      # Output a nicer version
      prettyNum(monthly_average, big.mark = ",", scientific = FALSE)
    })


    # Format for highcharter and apply SDC
    plot_df <- reactive({
      req(input$geography)
      req(input$sub_geography)
      req(input$count_or_percentage)

      # Rename column to value for highcharter motion
      sub_geography_df <- sub_geography_df() %>%
        # dplyr::rename(value = TOTAL_PATIENTS) %>%
        dplyr::mutate(
          value =
            dplyr::case_when(
              TOTAL_PATIENTS == 0 ~ 0,
              TOTAL_PATIENTS > 0 & TOTAL_PATIENTS <= 4 ~ NA_real_, # in the download button, it will be changed to 'c'
              TRUE ~ round(TOTAL_PATIENTS, -1)
            )
        )

      # If its a percentage then calculate it
      # round to the nearest numeric value
      if (input$count_or_percentage == "Percentage") {
        sub_geography_df <- sub_geography_df %>%
          dplyr::group_by(YEAR_MONTH) %>%
          dplyr::mutate(
            value = dplyr::case_when(
              TOTAL_PATIENTS == 0 ~ 0,
              TOTAL_PATIENTS > 0 & TOTAL_PATIENTS <= 4 ~ NA_real_,
              TRUE ~ round(TOTAL_PATIENTS / sum(TOTAL_PATIENTS) * 100, 0)
            ) # exclude if <=4 case as users can recalculate from the total count in the count toggle
          ) %>%
          dplyr::ungroup()
      }

      sub_geography_df
    })



    # need to do little tweak for the downloader as we can't plot c in the chart.
    # replace NA in value as 'c'

    download_df <- reactive({

      # Rename column to `Number of patients` for download

      selected_data_download <- plot_df() %>%
        dplyr::mutate(VALUE = dplyr::case_when(
          is.na(value) ~ "c",
          TRUE ~ as.character(value)
        )) %>%
        dplyr::select(-value, -TOTAL_PATIENTS)
    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste0(input$sub_geography, "_", input$count_or_percentage, "_", Sys.Date(), ".csv", sep = "")
      },
      content = function(con) {
        write.csv(download_df(), con)
      }
    )


    # Pull the max value
    max_value <- reactive({
      req(input$geography)
      req(input$sub_geography)
      req(input$count_or_percentage)

      max(plot_df()$value, na.rm = TRUE)
    })

    # Format for highcharter animation.
    plot_series_list <- reactive({
      req(input$geography)
      req(input$sub_geography)
      req(input$count_or_percentage)

      plot_df() %>%
        # Negate male values so the butterfly chart works
        dplyr::mutate(value = value * ifelse(GENDER == "Male", 1, -1)) %>%
        # Get all combinations of data
        tidyr::complete(YEAR_MONTH, AGE_BAND, GENDER,
          fill = list(value = 0)
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
    output$demographics <-
      highcharter::renderHighchart({
        req(input$geography)
        req(input$geography)
        req(input$count_or_percentage)

        # Create the chart
        chart <- highcharter::highchart() %>%
          highcharter::hc_chart(type = "bar", marginBottom = 100) %>%
          highcharter::hc_add_series_list(x = plot_series_list()) %>%
          highcharter::hc_motion(
            labels = unique(plot_df()$YEAR_MONTH),
            series = c(0, 1)
          ) %>%
          theme_nhsbsa(palette = "gender") %>%
          highcharter::hc_xAxis(
            title = list(text = "Age Band"),
            categories = sort(unique(plot_df()$AGE_BAND)),
            reversed = FALSE
          )

        if (input$count_or_percentage == "Count") {
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

    # Create the reactive text to go inside the chart
    output$text <- shiny::renderUI({
      req(input$geography)
      req(input$sub_geography)

      tagList(
        p(
          id = "medium",
          ifelse(input$sub_geography == "Overall", "", "In "),
          input$sub_geography, " we estimate", tags$b(female_ps()[1]), "of care ",
          "home patients are females and", tags$b(female_ps()[2]), "are female aged 85 ",
          "or over."
        ),
        p(
          id = "small",
          "Average number of monthly care home patients is",
          tags$b(paste0(average_monthly_patients(), "."))
        )
      )
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
        highcharter::hc_legend(enabled = F) %>%
        highcharter::hc_xAxis(
          categories = c(NA, "1<br>Most<br>deprived", 2:4, "5<br>Least<br>deprived"),
          title = list(text = "Deprivation quintile")
        ) %>%
        highcharter::hc_yAxis(
          title = list(text = "% of care home patients")
        ) %>%
        highcharter::hc_tooltip(
          shared = FALSE,
          formatter = highcharter::JS("function () { return '<b>Quintile: </b>' + parseInt(this.point.category) + ' (' + this.point.y + '%)'} ")
        )
    })

    return(
      reactive(download_df)
    )
  })
}

## To be copied in the UI
# mod_02_demographics_ui("02_demographics_ui_1")

## To be copied in the server
# mod_02_demographics_server("02_demographics_ui_1")
