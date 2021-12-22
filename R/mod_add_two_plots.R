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
    br(),
    br(),
    fluidRow(
      col_6(
        highcharter::highchartOutput(
          outputId = ns("rolling_line"),
          height = "600px",
          width = "600px"
        )
      )
    ),
    br(),
    br(),
    fluidRow(
      col_6(
        highcharter::highchartOutput(
          outputId = ns("comparison_line"),
          height = "500px",
          width = "900px"
        )
      )
    ),
    br(),
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
      dplyr::select(MONTH_YEAR) %>% 
      dplyr::distinct() %>% 
      dplyr::pull()
    
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
    
    # Load dfs for third plot
    rolling_pat <- careHomePrescribingScrollytellR::rolling_pat_count_df %>% 
      dplyr::arrange(FINAL_MONTH)
    
    # Month vector for actors
    month_vec <- rolling_pat %>% 
      dplyr::select(FINAL_MONTH, YEAR_MONTH) %>% 
      dplyr::distinct() %>% 
      dplyr::arrange(FINAL_MONTH) %>% 
      dplyr::select(YEAR_MONTH) %>% 
      dplyr::pull()
    
    # Line Chart
    output$rolling_line <- highcharter::renderHighchart({
      
      # Plot the data
      rolling_pat %>% 
        highcharter::hchart(
          type = "line",
          highcharter::hcaes(YEAR_MONTH, PATS, group = NUM_MONTH)
        ) %>% 
        highcharter::hc_yAxis(
          min = 0,
          title = list(text = "Distinct Patient Count")
        ) %>% 
        highcharter::hc_xAxis(
          title = list(text = "Year Month"),
          categories = month_vec
        ) %>% 
        highcharter::hc_title(
          text = "Single Month, Rolling 3-Month and Rolling 6-Month Distinct Patient 
        Counts, for any Care Home Classified Resident Receiving a Prescription"
        ) %>% 
        highcharter::hc_tooltip(table = T) %>% 
        highcharter::hc_credits(enabled = T)
    })
    
    # DF for plot
    chapter_comp <- careHomePrescribingScrollytellR::chapter_drug_comparison_df

    # Get descending difference vector of chapter names
    chapter_vec <- chapter_comp %>% 
      dplyr::mutate(
        CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home")
        ) %>% 
      tidyr::pivot_wider(names_from = "CH_FLAG", values_from = "PROP") %>% 
      dplyr::mutate(DIFF = `Care home` - `Non care home`) %>% 
      dplyr::arrange(desc(DIFF)) %>% 
      dplyr::select(CHAPTER_DESCR) %>% 
      dplyr::pull()
    
    # Process rssults ready for plot
    chapter_data <- chapter_comp %>% 
      dplyr::mutate(
        CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home")
        ) %>% 
      tidyr::pivot_wider(names_from = "CH_FLAG", values_from = "PROP") %>% 
      dplyr::mutate(
        DIFF = round(`Care home` - `Non care home`, 4) * 100,
        GREATER = dplyr::case_when(
          DIFF > 0 ~ "Care homes",
          DIFF < 0 ~ "Non-care homes",
          T ~ "Neither"),
        `Care home` = round(`Care home`, 4) * 100,
        `Non care home` = round(`Non care home`, 4) * 100,
        CHAPTER_DESCR = factor(CHAPTER_DESCR, levels = chapter_vec)
      ) %>% 
      dplyr::arrange(desc(DIFF))
    
    # Comparison Chart
    output$comparison_line <- highcharter::renderHighchart({
      
      highcharter::hchart(
        object = chapter_data,
        type = "spline",
        highcharter::hcaes(CHAPTER_DESCR, DIFF),
        color = "green"
        ) %>% 
        highcharter::hc_yAxis(plotBands = list(
          list(
            color = "#fff5f0",
            from = 0,
            to = -30
          ),
          list(
            color = "#f7fbff",
            from = 0,
            to = 30
          ))) %>% 
        highcharter::hc_yAxis(min = -20.5, max = 10.7, inverted = T) %>% 
        highcharter::hc_plotOptions(spline = list(marker = list(radius = 0))) %>% 
        highcharter::hc_xAxis(title = list(text = "BNF Chapter Description")) %>% 
        highcharter::hc_yAxis(title = list(
          text = "Difference between Care home and <br> non-care home proportions")
        ) %>% 
        highcharter::hc_title(
          text = "<b>The Difference in BNF Chapter-level Prescribing 
        in Care Home and non-Care Homes<b/>"
        ) %>% 
        highcharter::hc_subtitle(
          text = "Proportions calculated by looking at chapter prescribing against 
        all prescribing<br>either on a care home or non-care home-level"
        ) %>% 
        highcharter::hc_tooltip(
          headerFormat = "",
          pointFormat = "<b>BNF Chapter Description:</b> {point.CHAPTER_DESCR}<br>
        <b>Proportion of Care Home Prescribing:</b> {point.Care home} %<br>
        <b>Proportion of Non-Care Home Prescribing:</b> {point.Non care home} %<br>
        <b>Proportion Difference:</b> {point.DIFF} %<br>
        <b>Proportion Greater in:</b> {point.GREATER}"
        ) %>% 
        highcharter::hc_chart(inverted = T) %>% 
        highcharter::hc_credits(enabled = T) %>% 
        highcharter::hc_annotations(
          list(
            labels = list(
              list(
                point = list(x = 200, y = 200),
                text = "Greater proportion of <br>items in non-care homes"
              ),
              list(
                point = list(x = 200, y = 450),
                text = "Greater proportion of <br>items in care homes"
              )
            )
          )
        )
    })
    
  })
}
    