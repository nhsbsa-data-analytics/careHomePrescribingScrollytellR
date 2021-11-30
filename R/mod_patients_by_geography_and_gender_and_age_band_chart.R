#' patients_by_geography_and_gender_and_age_band_chart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_patients_by_geography_and_gender_and_age_band_chart_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;",
      h6(
        "Age band and gender of estimated older care home patients in England ",
        "(2020/21)"
      ),
      col_6(
        selectInput(
          inputId = ns("level"),
          label = "Level",
          choices = c("Overall", "Region", "STP", "Local Authority"),
          width = "100%"
        )
      ),
      col_6(
        selectInput(
          inputId = ns("geography"),
          label = "Geography",
          choices = NULL, # dynamically generated
          width = "100%"
        )
      ),
      highcharter::highchartOutput(
        outputId = ns("patients_by_geography_and_gender_and_age_band_chart"),
        height = "500px",
        width = "800px"
      )
    )
  )
}

#' patients_by_geography_and_gender_and_age_band_chart Server Function
#'
#' @noRd
mod_patients_by_geography_and_gender_and_age_band_chart_server <- function(
  input, 
  output, 
  session
) {
  ns <- session$ns
  
  # Filter to relevant data for this chart
  patients_by_geography_and_gender_and_age_band_df <- 
    careHomePrescribingScrollytellR::patients_by_geography_and_gender_and_age_band_df %>%
    dplyr::filter(dplyr::across(c(LEVEL, GEOGRAPHY, PDS_GENDER), not_na))
    
  # Handy resource: https://mastering-shiny.org/action-dynamic.html
  
  # Filter the data based on the level
  level_df <- reactive({
    req(input$level)
    patients_by_geography_and_gender_and_age_band_df %>%
      dplyr::filter(LEVEL == input$level)
    
  })
  
  # Update the list of choices for geography from the rows in level dataframe
  observeEvent(
    eventExpr = level_df(), 
    handlerExpr = {
      updateSelectInput(
        inputId = "geography", 
        choices = unique(level_df()$GEOGRAPHY)
      ) 
    }
  )
  
  # Filter the data based on the level and format for the plot
  plot_df <- reactive({
    req(input$geography)
    level_df() %>%
      dplyr::filter(GEOGRAPHY == input$geography) %>%
      dplyr::group_by(YEAR_MONTH) %>%
      dplyr::mutate(p = TOTAL_PATIENTS / sum(TOTAL_PATIENTS) * 100) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(p = p * ifelse(PDS_GENDER == "Male", 1, -1))
  })
  
  # Pull the max p
  max_p <- reactive({
    req(input$geography)
    max(abs(plot_df()$p))
  })
  
  # Format for highcharter animation
  plot_series_list <- reactive({
    req(input$geography)
    plot_df() %>%
      tidyr::expand(YEAR_MONTH, AGE_BAND, PDS_GENDER) %>%
      dplyr::left_join(plot_df()) %>%
      dplyr::mutate(p = tidyr::replace_na(p)) %>%
      dplyr::group_by(AGE_BAND, PDS_GENDER) %>%
      dplyr::do(data = list(sequence = .$p)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(PDS_GENDER) %>%
      dplyr::do(data = .$data) %>%
      dplyr::mutate(name = PDS_GENDER) %>%
      highcharter::list_parse()
    
  })

  # Pyramid plot for age band and gender
  output$patients_by_geography_and_gender_and_age_band_chart <- highcharter::renderHighchart({
    
    req(input$geography)
    
    highcharter::highchart() %>%
      highcharter::hc_chart(type = "bar", marginBottom = 100) %>%
      highcharter::hc_add_series_list(x = plot_series_list()) %>%
      highcharter::hc_motion(
        labels = unique(plot_df()$YEAR_MONTH),
        series = c(0, 1)
      ) %>%
      theme_nhsbsa(palette = "gender") %>%
      highcharter::hc_xAxis(
        categories = sort(unique(plot_df()$AGE_BAND)),
        reversed = FALSE
      ) %>%
      highcharter::hc_yAxis(
        min = -ceiling(max_p() / 5) * 5,
        max = ceiling(max_p() / 5) * 5,
        labels = list(
          formatter = highcharter::JS("function(){ return Math.abs(this.value) + '%' ;}")
        )
      ) %>%
      highcharter::hc_tooltip(
        shared = FALSE,
        formatter = highcharter::JS("function () { return '<b>Gender: </b>' + this.series.name + '<br>' + '<b>Age band (5 years): </b>' + this.point.category + '<br/>' + '<b>Percentage: </b>' + Math.abs(Math.round(this.point.y * 10) / 10) + '%';}")
      )
  })
}

## To be copied in the UI
# mod_patients_by_geography_and_gender_and_age_band_chart_ui("patients_by_geography_and_gender_and_age_band_chart_1")

## To be copied in the server
# callModule(mod_patients_by_geography_and_gender_and_age_band_chart_server, "patients_by_geography_and_gender_and_age_band_chart_1")
