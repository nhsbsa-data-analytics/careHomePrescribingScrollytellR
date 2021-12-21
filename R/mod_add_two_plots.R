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
    fluidRow(
      col_6(
        highcharter::highchartOutput(
          outputId = ns("patient_area"),
          height = "700px",
          width = "500px"
          )
        )
      ),
    br(),
    br(),
    fluidRow(
      col_6(
        highcharter::highchartOutput(
          outputId = ns("cost_line"),
          height = "700px",
          width = "500px"
        )
      )
    ),
    br()
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
          highcharter::hcaes(MONTH_YEAR, n, group = CH_FLAG),
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
          highcharter::hcaes(MONTH_YEAR, n, group = CH_FLAG),
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
        highcharter::hc_title(text = "Monthly Prescribing Status of All Patients 
        who Received at Least One Care Home Prescription in FY 2020/21") %>% 
        highcharter::hc_plotOptions(
          series = list(
            events = list(
              legendItemClick = htmlwidgets::JS("function(e){e.preventDefault();}")
            ))) %>% 
        highcharter::hc_credits(enabled = T)
    }) 
    
    # Load df for second plot
    cost_age_gender_ch <- careHomePrescribingScrollytellR::cost_by_age_gender_and_ch_flag_df %>% 
      as.data.frame()
    
    # Line Chart
    output$cost_line <- highcharter::renderHighchart({
      
      # Highchart
      highcharter::hchart(
        object = cost_age_gender_ch,
        type = "line",
        highcharter::hcaes(x = AGE_BAND, y = COST_PER_PATIENT, group = METRIC)
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
          pointFormat = "<b>{point.METRIC}:</b> £{point.COST_PER_PATIENT:,.2f}"
        ) %>% 
        highcharter::hc_credits(enabled = T) %>% 
        highcharter::hc_colors(c("darkgreen", "lightgreen", "darkblue", "lightblue"))
      
    })
    
  })
}
    