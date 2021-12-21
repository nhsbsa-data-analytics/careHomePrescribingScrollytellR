#' add_two_plots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_add_two_plots_ui <- function(id){
  ns <- NS(id)
  tagList(
    col_6(
      highcharter::highchartOutput(
        outputId = ns("patient_area"),
        height = "500px",
        width = "900px"
      )
    ),
    col_6(
      highcharter::highchartOutput(
        outputId = ns("cost_line"),
        height = "500px",
        width = "900px"
      )
    )
  )
}
    
#' add_two_plots Server Functions
#'
#' @noRd 
mod_add_two_plots_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Loads 2 dfs
    with_prescribing <- careHomePrescribingScrollytellR::monthly_prescribing_status_with_prescribing_df
    without_prescribing <- careHomePrescribingScrollytellR::monthly_prescribing_status_without_prescribing_df
    
    # x-Axis Categories
    month_vec <- with_prescribing %>% 
      select(MONTH_YEAR) %>% 
      distinct() %>% 
      pull()
    
    # Highchart
    output$patient_area <- highcharter::renderHighchart({
      
      highcharter::highchart() %>% 
        highcharter::hc_xAxis(categories = month_vec) %>% 
        highcharter::hc_add_series(
          data = with_prescribing,
          type = "area",
          hcaes(MONTH_YEAR, n, group = CH_FLAG),
          yAxis = 0
        ) %>% 
        highcharter::hc_add_yAxis(
          nid = 1L,
          title = list(text = "No. of patients receiving prescribing (k)"),
          relative = 2.5
        ) %>% 
        highcharter::hc_add_series(
          data = without_prescribing,
          type = "area",
          hcaes(MONTH_YEAR, n, group = CH_FLAG),
          yAxis = 1,
          legend = list(reversed = T)
        ) %>% 
        highcharter::hc_add_yAxis(
          nid = 2L,
          title = list(
            text = "No. of patients <b>not</b> receiving prescribing (k)"
          ),
          relative = 1,
          opposite = T,
          chart = list(inverted = T),
          reversed = T
        ) %>% 
        highcharter::hc_plotOptions(
          area = list(
            stacking = "total"
          )
        ) %>% 
        highcharter::hc_tooltip(
          pointFormat = '<b>Prescribing Status:</b> {point.CH_FLAG}<br>
        <b>Number of Patients:</b> {point.n} k'
        ) %>% 
        highcharter::hc_legend(layout = "vertical") %>% 
        highcharter::hc_title(text = "Monthly Prescribing Status of All Patients who Received 
               at Least One Care Home Prescription in FY 2020/21") %>% 
        highcharter::hc_plotOptions(
          series = list(
            events = list(
              legendItemClick = htmlwidgets::JS("function(e){e.preventDefault();}")
            )))
    }) 
      
  })
}
    