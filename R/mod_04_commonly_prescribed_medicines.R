#' 04_commonly_prescribed_medicine UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_04_commonly_prescribed_medicines_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Commonly prescribed medicines"),
    br(),
    h6(
      "The range of medicines prescribed to older care home patients differs ",
      "significantly to older non-care home patients."
    ),
    p(
      "Care home patients are more likely to receive", 
      tags$b("drugs for pain relief"), "than non-care home patients, in terms ",
      "of number of prescription items and patients receiving them. Whilst",
      tags$b("nutrition products"), "account for a greater percentage of drug ",
      "cost."
    ),
    p(
      "The chart below allows you to view and compare the 50 most common ",
      "medicines prescribed to older care home patients and older non-care ",
      "home patients across three prescribing metrics."
    ),
    p(
      "Medicines have been identified using a",
      tippy(
        text = "BNF code and are grouped at 4 levels.",
        tooltip = tooltip_text$bnf_code
      )
    ),
    fluidRow(
      style = "background-color: #FFFFFF;",
      align = "center",
      h6(
        "Most common medicines prescribed to older care home and non-care ",
        "home patients in England (2020/21)"
      ),
      col_4(
        selectInput(
          inputId = ns("bnf"),
          label = "BNF Level",
          choices = names(careHomePrescribingScrollytellR::bnfs),
          width = "100%"
        )
      ),
      col_4(
        selectInput(
          inputId = ns("metric"),
          label = "Metric",
          choices = c(
            "Drug cost" = "COST",
            "Number of prescription items" = "ITEMS",
            "Number of patients" = "PATIENTS"
          ),
          width = "100%"
        )
      ),
      col_4(
        selectInput(
          inputId = ns("sort"),
          label = "Sort by",
          choices = c(
            "Care home" = "PCT_CH",
            "Non-care home" = "PCT_NON_CH"
          ),
          width = "100%"
        )
      ),
      shiny::uiOutput(outputId = ns("text")),
      highcharter::highchartOutput(
        outputId = ns("metrics_by_bnf_and_ch_flag_chart"),
        height = "400px"
      )
    ),
    mod_download_ui(
      id = ns("download_metrics_by_bnf_and_ch_flag_chart")
    )
  )
}

#' 04_commonly_prescribed_medicine Server Functions
#'
#' @noRd
mod_04_commonly_prescribed_medicines_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Filter data to BNF level, metric, and sort (taking the top 50)
    metrics_by_bnf_and_ch_flag_df <- reactive({
      req(input$bnf)
      req(input$metric)
      req(input$sort)

      careHomePrescribingScrollytellR::metrics_by_bnf_and_ch_flag_df %>%
        dplyr::filter(
          BNF_LEVEL == input$bnf,
          METRIC == input$metric
        ) %>%
        dplyr::arrange(desc(.data[[input$sort]])) %>%
        head(50) %>%
        dplyr::select(-c(PCT_CH, PCT_NON_CH))
    })

    # Swap NAs for "c" for data download
    metrics_by_bnf_and_ch_flag_download_df <- reactive({
      req(input$bnf)
      req(input$metric)
      req(input$sort)

      metrics_by_bnf_and_ch_flag_df() %>%
        dplyr::mutate(
          SDC_PCT_CH = ifelse(
            test = is.na(SDC_PCT_CH),
            yes = "c",
            no = as.character(SDC_PCT_CH)
          ),
          SDC_PCT_NON_CH = ifelse(
            test = is.na(SDC_PCT_NON_CH),
            yes = "c",
            no = as.character(SDC_PCT_NON_CH)
          )
        )
    })

    # Add a download button
    mod_download_server(
      id = "download_metrics_by_bnf_and_ch_flag_chart",
      filename = "metrics_by_bnf_and_ch_flag_df.csv",
      export_data = metrics_by_bnf_and_ch_flag_download_df()
    )

    # Create the chart
    output$metrics_by_bnf_and_ch_flag_chart <- highcharter::renderHighchart({
      req(input$bnf)
      req(input$metric)
      req(input$sort)

      highcharter::highchart() %>%
        highcharter::hc_add_series(
          data = metrics_by_bnf_and_ch_flag_df(),
          type = "dumbbell",
          highcharter::hcaes(
            low = SDC_PCT_NON_CH,
            high = SDC_PCT_CH
          ),
          lowColor = "#768692",
          color = "#768692",
          marker = list(fillColor = "#005EB8")
        ) %>%
        highcharter::hc_subtitle(
          useHTML = TRUE,
          text =
            "
            <span style = 'color:#005EB8; font-size: 20px'> &bull; </span> <b> <span style = font-size: 35px'> Care home </span> </b>
            <span style = 'color:#768692; font-size: 20px'> &bull; </span> <b> <span style = font-size: 35px'> Non-care home </span>
            ",
          align = "center"
        ) %>%
        highcharter::hc_chart(
          inverted = TRUE,
          marginLeft = 350
        ) %>%
        highcharter::hc_scrollbar(enabled = TRUE) %>%
        theme_nhsbsa() %>%
        highcharter::hc_caption(
          text = "Figures are calculated as a percentage of the care home or non-care home group and where the number of patients is less than 5 the data has been redacted."
        ) %>%
        highcharter::hc_xAxis(
          categories = unique(metrics_by_bnf_and_ch_flag_df()$SUB_BNF_LEVEL_NAME),
          style = list(
            fontSize = 15
          ),
          title = list(text = paste("BNF", input$bnf)),
          max = 14 # it shows n + 1 = 15
        ) %>%
        highcharter::hc_yAxis(
          min = 0,
          max = max(
            max(metrics_by_bnf_and_ch_flag_df()$SDC_PCT_NON_CH, na.rm = TRUE),
            max(metrics_by_bnf_and_ch_flag_df()$SDC_PCT_CH, na.rm = TRUE)
          ),
          title = list(
            text = switch(input$metric,
              "COST" = "Drug cost (%)",
              "ITEMS" = "Number of prescription items (%)",
              "PATIENTS" = "Number of patients (%)"
            )
          )
        ) %>%
        highcharter::hc_legend(enabled = FALSE) %>%
        highcharter::hc_tooltip(
          useHTML = TRUE,
          formatter = htmlwidgets::JS(
            "
            function() {

              outHTML =
                '<b>' + this.point.SUB_BNF_LEVEL_NAME + '</b> <br>' +
                'Older care home patients: ' + '<b>' + this.point.high + '%' + '</b> <br>' +
                'Older non-care home patients: ' + '<b>' + this.point.low + '%' + '</b>'

              return(outHTML)

            }
            "
          )
        )
    })
    
    # Create dynamic text paragraph - top 2 care home and top 1 non-care home
    output$text <- shiny::renderUI({
      req(input$bnf)
      req(input$metric)
      
      top_care_home_df <- metrics_by_bnf_and_ch_flag_df() %>%
        dplyr::arrange(desc(SDC_PCT_CH)) %>%
        head(2)
      
      top_non_care_home_df <- metrics_by_bnf_and_ch_flag_df() %>%
        dplyr::arrange(desc(SDC_PCT_NON_CH)) %>%
        head(1)
        
      col_12(
        class = "highcharts-caption",
        style = "margin-left: 1%; margin-right: 1%; text-align: left;",
        tags$b(top_care_home_df[1, "SUB_BNF_LEVEL_NAME"]), "and",
        tags$b(top_care_home_df[2, "SUB_BNF_LEVEL_NAME"]), "are the most ",
        "commonly prescribed BNF", paste0(input$bnf, "s"), " by percentage of ",
        switch(input$metric,
               "COST" = "drug cost ",
               "ITEMS" = "prescription items",
               "PATIENTS" = "patients prescribed"
        ),
        " in 2020/21, accounting for ",
        paste0(top_care_home_df[1, "SDC_PCT_CH"], "%"), " and ",
        paste0(top_care_home_df[1, "SDC_PCT_NON_CH"], "%"), " of all ",
        switch(input$metric,
               "COST" = "drug cost to",
               "ITEMS" = "prescription items to"
        ),
        " older care home patients. For non-care home patients it is",
        tags$b(top_non_care_home_df$SUB_BNF_LEVEL_NAME),
        paste0("(", top_non_care_home_df$SDC_PCT_NON_CH, "%).")
      )
      
    })
    
  })
}

## To be copied in the UI
# mod_04_commonly_prescribed_medicines_ui("04_commonly_prescribed_medicines_ui_1")

## To be copied in the server
# mod_04_commonly_prescribed_medicines_server("04_commonly_prescribed_medicines_ui_1")
