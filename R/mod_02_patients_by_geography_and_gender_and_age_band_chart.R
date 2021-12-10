#' 02_patients_by_geography_and_gender_and_age_band_chart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_patients_by_geography_and_gender_and_age_band_chart_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Demographic estimates for older care home patients receiving prescriptions"),
    p(
      "Overall, we estimate a monthly average of ", tags$b("284 thousand care home patients,"),
      " aged 65+ years receiving prescriptions each month, which accounts for around", tags$b("X%"), "of patients aged 65+ ",
      "years receiving prescription items."
    ),
    p(
      "Overall, ", tags$b("two thirds"), " are ", tags$b("female"), "(66%) and", tags$b("44%"), "are", tags$b("female aged 85+ years.")
    ),
    p(
      "The age and gender profile is broadly comparable to",
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
          choices = c("Overall", "Region", "STP", "Local Authority"),
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
        choices = c("Percentage", "Count"),
        inline = TRUE,
        width = "100%"
      ),
      highcharter::highchartOutput(
        outputId = ns("patients_by_geography_and_gender_and_age_band_chart"),
        height = "500px",
        width = "800px"
      )
    ),
    br(),
    br(),
    p(
      "We have used CQC data to identify residential care homes and nursing homes. On average we ",
      "estimate there around each month are similar numbers of older patients in residential (113 thousand) ",
      "and nursing (106 thousand) homes, with a small number of care home patients appearing in both (6.9 thousand) ",
      "during 2020/21."
    ),
    p(
      "Care home patient's prescription were allocated an IMD and associated decile/quintile based on area in which ",
      "the care home is located. On average, the proportion of XXX in each decile/quintile ",
      "is broadly in line with expected proportions. (note: will change depends on decile/quintile)"
    )
  )
}

#' patients_by_geography_and_gender_and_age_band_chart Server Function
#'
#' @noRd
mod_02_patients_by_geography_and_gender_and_age_band_chart_server <- function(
  input,
  output,
  session
) {
  ns <- session$ns

  # comma separate setting
  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- ","
  options(highcharter.lang = hcoptslang)

  # Radio button added as we cannot add two values in one sequence for the hc_motion
  metric_selection <- reactiveValues(v = NULL)

  observe({
    metric_selection$v <- input$count_or_percentage
  })

  # create as reactive value - now it holds selected value
  input_metric <- reactive(metric_selection$v)
  
  # Filter to relevant data for this chart
  patients_by_geography_and_gender_and_age_band_df <- 
    patients_by_geography_and_gender_and_age_band_df %>%
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
      updateSelectInput(
        inputId = "sub_geography", 
        choices = unique(geography_df()$SUB_GEOGRAPHY_NAME)
      ) 
    }
  )
  
  # Filter the data based on the sub geography and format for the plot
  plot_df <- reactive({
    req(input$geography)
    req(input$sub_geography)
    req(input$count_or_percentage)
    if (input$count_or_percentage == "Percentage") {
      geography_df() %>%
        dplyr::filter(SUB_GEOGRAPHY_NAME == input$sub_geography) %>%
        dplyr::group_by(YEAR_MONTH) %>%
        dplyr::mutate(value = TOTAL_PATIENTS / sum(TOTAL_PATIENTS) * 100) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(value = value * ifelse(GENDER == "Male", 1, -1))
    } else if (input$count_or_percentage == "Count") {
      geography_df() %>%
        dplyr::filter(SUB_GEOGRAPHY_NAME == input$sub_geography) %>%
        dplyr::group_by(YEAR_MONTH) %>%
        dplyr::mutate(value = TOTAL_PATIENTS) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(value = value * ifelse(GENDER == "Male", 1, -1))
    }
  })

  # Pull the max value
  max_value <- reactive({
    req(input$geography)
    req(input$sub_geography)
    req(input$count_or_percentage)
    max(abs(plot_df()$value), na.rm = TRUE)
  })

  # Format for highcharter animation
  # unfortunately, tooltip with two different values doesn't work.
  # change to toggle
  plot_series_list <- reactive({
    req(input$geography)
    req(input$sub_geography)
    req(input$count_or_percentage)
    plot_df() %>%
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
  output$patients_by_geography_and_gender_and_age_band_chart <- highcharter::renderHighchart({
    req(input$geography)
    req(input$geography)
    req(input$count_or_percentage)
    if (input$count_or_percentage == "Percentage") {
      highcharter::highchart() %>%
        highcharter::hc_chart(type = "bar", marginBottom = 100) %>%
        highcharter::hc_add_series_list(x = plot_series_list()) %>%
        highcharter::hc_motion(
          labels = unique(plot_df()$YEAR_MONTH),
          series = c(0, 1)
        ) %>%
        theme_nhsbsa(palette = "gender") %>%
        highcharter::hc_xAxis(
          title = list(text = "Age group"),
          categories = sort(unique(plot_df()$AGE_BAND)),
          reversed = FALSE
        ) %>%
        highcharter::hc_yAxis(
          title = list(text = "Number of care home patients as percentage of all care home patients (%)"),
          min = -ceiling(max_value() / 5) * 5,
          max = ceiling(max_value() / 5) * 5,
          labels = list(
            formatter = highcharter::JS("function(){ return Math.abs(this.value);}")
          )
        ) %>%
        highcharter::hc_tooltip(
          shared = FALSE,
          formatter = highcharter::JS("function () { return '<b>Gender: </b>' + this.series.name + '<br>' + '<b>Age band (5 years): </b>' + this.point.category + '<br/>' + '<b>Percentage: </b>' + Math.abs(this.point.y).toFixed(1) + '%';}")
        )
    } else {
      highcharter::highchart() %>%
        highcharter::hc_chart(type = "bar", marginBottom = 100) %>%
        highcharter::hc_add_series_list(x = plot_series_list()) %>%
        highcharter::hc_subtitle(
          text = "Note: Counts rounded to nearest ten (e.g. 12 rounded to 10) "
        ) %>%
        highcharter::hc_motion(
          labels = unique(plot_df()$YEAR_MONTH),
          series = c(0, 1)
        ) %>%
        theme_nhsbsa(palette = "gender") %>%
        highcharter::hc_xAxis(
          title = list(text = "Age group"),
          categories = sort(unique(plot_df()$AGE_BAND)),
          reversed = FALSE
        ) %>%
        highcharter::hc_yAxis(
          title = list(text = "Number of care home patients"),
          min = -ceiling(max_value() / 5) * 5,
          max = ceiling(max_value() / 5) * 5,
          labels = list(
            formatter = htmlwidgets::JS("function(){ return Math.abs(this.value)/1000 + 'K';}")
          )
        ) %>%
        highcharter::hc_tooltip(
          shared = FALSE,
          useHTML = TRUE,
          formatter = htmlwidgets::JS(
            "
            function(){
            if(Math.abs(this.point.y) >= 1000){
            outHTML = '<b>Gender: </b>' + this.series.name + '<br>' + '<b>Age band (5 years): </b>' + this.point.category + '<br/>' + '<b>Number of patients: </b>' + Highcharts.numberFormat(Math.abs(Math.round(this.point.y/10)*10),0)
            return(outHTML)
            }else{
            outHTML = '<b>Gender: </b>' + this.series.name + '<br>' + '<b>Age band (5 years): </b>' + this.point.category + '<br/>' + '<b>Number of patients: </b>' + Math.abs(Math.round(this.point.y /10)*10)
            return(outHTML)
            }
            }
            "
          )
        )
    }
  })
}

## To be copied in the UI
# mod_02_patients_by_geography_and_gender_and_age_band_chart_ui("02_patients_by_geography_and_gender_and_age_band_chart_1")

## To be copied in the server
# callModule(mod_03_patients_by_geography_and_gender_and_age_band_chart_server, "02_patients_by_geography_and_gender_and_age_band_chart_1")
