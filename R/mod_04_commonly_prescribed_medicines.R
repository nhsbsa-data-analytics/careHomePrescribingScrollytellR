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
    p(
      "The range of medicines prescribed to older care home patients compared ",
      "to older non-care home patients differs significantly."
    ),
    p(
      "Each prescribed medicine is attributed a BNF code that defines a ",
      "heirachy. We can look at prescribing by either the total drug cost, ",
      "total number of items, or the total number of patients prescribed a ",
      "medicine."
    ),
    p("In England in 2020/21:"),
    tags$ul(
      tags$li(
        style = "font-size: 16pt;",
        tags$b("BNF Chapter:"), "91% of older care home patients recieved at ",
        "least one prescription item from the Central Nervous System compared ",
        "to just under half (49%) of non-care home patients."
      ),
      tags$li(
        style = "font-size: 16pt;",
        tags$b("BNF Section:"), "Oral nutrition products account for the ",
        "greatest percentage (14%) of the drug cost in older care home ",
        "patients, whereas for older non-care home patients it makes up just ",
        "2% of the drug cost."
      ),
      tags$li(
        style = "font-size: 16pt;",
        tags$b("BNF Paragraph:"), "Enteral nutrition makes up 13% of the drug ",
        "cost in older care home patients but only accounts for 2% of the ",
        "drug cost for non-care home patients."
      ),
      tags$li(
        style = "font-size: 16pt;",
        tags$b("BNF Chemical Substance:"), "Paracetamol makes up 5% of ",
        "prescription items for older care home patients, whereas it makes up ",
        "just 2% of prescription items for older non-care home patients. 64% ",
        "of older care home patients recieved at least one prescription ",
        "prescription item for Paracetamol."
      )
    ),
    p(
      "The tool below allows you to compare prescribing to older care home ",
      "patients against older non-care home patients at four BNF levels and ",
      "across three metrics in more detail. You can use the scrollbar to view ",
      "the most common 50 medicines sorted by either care home or non-care ",
      "home."
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
      highcharter::highchartOutput(
        outputId = ns("metrics_by_bnf_and_ch_flag_chart"),
        height = "350px"
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
          text = "Figures are calculated as a percentage of the care home or non-care home group.",
          align = "right"
        ) %>%
        highcharter::hc_xAxis(
          categories = unique(metrics_by_bnf_and_ch_flag_df()$SUB_BNF_LEVEL_NAME),
          style = list(
            fontSize = 15
          ),
          title = list(text = paste("BNF", input$bnf)),
          max = 10
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


  })
}

## To be copied in the UI
# mod_04_commonly_prescribed_medicines_ui("04_commonly_prescribed_medicines_ui_1")

## To be copied in the server
# mod_04_commonly_prescribed_medicines_server("04_commonly_prescribed_medicines_ui_1")
