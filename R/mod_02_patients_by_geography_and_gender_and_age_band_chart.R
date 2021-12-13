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
      " aged 65+ years receiving prescriptions each month, which accounts for around", tags$b("4%"), "of patients aged 65+ ",
      "years receiving prescription items."
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
      col_8(
        highcharter::highchartOutput(
          outputId = ns("patients_by_geography_and_gender_and_age_band_chart"),
          height = "500px",
          width = "900px"
        )
      ),
      col_4(
        br(),
        br(),
        shiny::htmlOutput(
          ns("text")
        )
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
mod_02_patients_by_geography_and_gender_and_age_band_chart_server <- function(input,
                                                                              output,
                                                                              session) {
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

  # pull % of female of selected geography (# female / total patients)
  female_p <- reactive({
    req(input$geography)
    req(input$sub_geography)
    geography_df() %>%
      dplyr::filter(SUB_GEOGRAPHY_NAME == input$sub_geography & YEAR_MONTH == "Overall") %>%
      dplyr::group_by(GENDER) %>%
      dplyr::summarise(PAT_SUM = sum(TOTAL_PATIENTS)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(P = floor(PAT_SUM / sum(PAT_SUM) * 100)) %>%
      dplyr::filter(GENDER == "Female") %>%
      dplyr::select(P) %>%
      dplyr::pull()
  })


  # pull % of 85+ female of selected geography (# 85+ female / # total patients)
  female_85plus_p <- reactive({
    req(input$geography)
    req(input$sub_geography)
    geography_df() %>%
      dplyr::filter(SUB_GEOGRAPHY_NAME == input$sub_geography & YEAR_MONTH == "Overall") %>%
      dplyr::mutate(GENDER_RECODE = dplyr::case_when(
        GENDER == "Female" & AGE_BAND == "85-89" ~ "Female_85_plus",
        GENDER == "Female" & AGE_BAND == "90+" ~ "Female_85_plus",
        TRUE ~ "Other"
      )) %>%
      dplyr::group_by(GENDER_RECODE) %>%
      dplyr::summarise(PAT_SUM = sum(TOTAL_PATIENTS)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(P = floor(PAT_SUM / sum(PAT_SUM) * 100)) %>%
      dplyr::filter(GENDER_RECODE == "Female_85_plus") %>%
      dplyr::select(P) %>%
      dplyr::pull()
  })

  # pull monthly average of care home patients
  avg_pats <- reactive({
    req(input$geography)
    req(input$sub_geography)
    geography_df() %>%
      dplyr::filter(SUB_GEOGRAPHY_NAME == input$sub_geography & YEAR_MONTH != "Overall") %>%
      dplyr::group_by(YEAR_MONTH) %>%
      dplyr::summarise(MONTHLY_TOTAL_PAT = sum(TOTAL_PATIENTS)) %>%
      dplyr::summarise(AVG_PAT = ceiling(sum(MONTHLY_TOTAL_PAT) / 12 / 1000) * 1000) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(AVG_PAT = prettyNum(AVG_PAT, big.mark = ",", scientific = FALSE)) %>%
      dplyr::pull()
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
          title = list(text = "Estimated number of care home patients (thousands)"),
          min = -ceiling(max_value() / 5) * 5,
          max = ceiling(max_value() / 5) * 5,
          labels = list(
            formatter = htmlwidgets::JS("function(){ return Math.abs(this.value)/1000;}")
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

  output$text <- shiny::renderUI({
    if (input$sub_geography == "Overall") {
      shiny::HTML(paste(
        '<p id = "small"; style = "border:1px; border-color:#808080;padding:2em">',
        " Overall we estimate ", "<b>", female_p(),
        "%</b>", " of care home patients are females and ", "<b>", female_85plus_p(),
        "%</b>", " are female aged 85+ years.", "</p>", "<br> <br> <br> <br> <br>",
        '<p id = "small"; style = "border:1px; border-color:#808080;padding:2em">', "<br>", "Monthly average of", " overall care home patients are ", "<b>",
        avg_pats(), "</b>", ". </p>",
        sep = ""
      ))
    } else {
      shiny::HTML(paste(
        '<p id = "small"; style = "border:1px; border-color:#808080;padding:2em">', "In ",
        input$sub_geography, ", we estimate ", "<b>", female_p(),
        "%</b>", " of care home patients are females and ", "<b>", female_85plus_p(),
        "%</b>", " are female aged 85+ years.", "</p>", "<br> <br> <br> <br> <br>",
        '<p id = "small"; style = "border:1px; border-color:#808080;padding:2em">', "<br>", "Monthly average of ", input$sub_geography, " care home patients are ", "<b>",
        avg_pats(), "</b>", ". </p>",
        sep = ""
      ))
    }
  })
}

## To be copied in the UI
# mod_02_patients_by_geography_and_gender_and_age_band_chart_ui("02_patients_by_geography_and_gender_and_age_band_chart_1")

## To be copied in the server
# callModule(mod_03_patients_by_geography_and_gender_and_age_band_chart_server, "02_patients_by_geography_and_gender_and_age_band_chart_1")
