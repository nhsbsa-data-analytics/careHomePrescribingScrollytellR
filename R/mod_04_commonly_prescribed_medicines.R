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
    h2("Commonly prescribed medicines"),
    p(
      tags$b(
        "The range of medicines prescribed to care home patients aged 65 year or over",
        "differs significantly to non-care home patients aged 65 years or over."
      )
    ),
    p(
      "Care home patients are more likely to receive",
      tags$b("drugs for pain relief"), "than non-care home patients, in terms ",
      "of number of prescription items and patients receiving them. Whilst",
      tags$b("nutrition products"), "account for a greater percentage of drug ",
      "cost."
    ),
    p(
      "The chart below allows you to view and compare the 20 most common ",
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
    nhs_card(
      heading = "Most common medicines prescribed to care home patients aged 65 years or over and non-care home patients in England (2020/21)",
      nhs_grid_3_col(
        nhs_selectInput(
          inputId = ns("bnf"),
          label = "BNF Level",
          choices = names(careHomePrescribingScrollytellR::bnfs),
          full_width = TRUE
        ),
        nhs_selectInput(
          inputId = ns("metric"),
          label = "Metric",
          choices = c(
            "Drug cost (PPM)" = "COST",
            "Number of prescription items (PPM)" = "ITEMS"
          ),
          full_width = TRUE
        ),
        nhs_selectInput(
          inputId = ns("sort"),
          label = "Sort by",
          choices = c(
            "Care home" = "PPM_CH",
            "Non-care home" = "PPM_NON_CH"
          ),
          full_width = TRUE
        )
      ),
      shiny::uiOutput(outputId = ns("text")),
      highcharter::highchartOutput(
        outputId = ns("metrics_by_bnf_and_ch_flag_chart"),
        height = "500px"
      ),
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "Figures are calculated as a patient per month (PPM)."
      ),
      mod_nhs_download_ui(
        id = ns("download_metrics_by_bnf_and_ch_flag_chart")
      )
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
        # head(50) %>%
        dplyr::select(-c(PPM_CH, PPM_NON_CH))
    })

    # Swap NAs for "c" for data download
    metrics_by_bnf_and_ch_flag_download_df <- reactive({
      req(input$bnf)
      req(input$metric)
      req(input$sort)

      metrics_by_bnf_and_ch_flag_df() %>%
        dplyr::mutate(
          SDC_PPM_CH = ifelse(
            test = is.na(SDC_PPM_CH),
            yes = "c",
            no = as.character(SDC_PPM_CH)
          ),
          SDC_PCT_NON_CH = ifelse(
            test = is.na(SDC_PPM_NON_CH),
            yes = "c",
            no = as.character(SDC_PPM_NON_CH)
          )
        )
    })

    # Add a download button
    mod_nhs_download_server(
      id = "download_metrics_by_bnf_and_ch_flag_chart",
      filename = "metrics_by_bnf_and_ch_flag_df.csv",
      export_data = metrics_by_bnf_and_ch_flag_download_df
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
            low = SDC_PPM_NON_CH,
            high = SDC_PPM_CH
          ),
          lowColor = "#425563",
          color = "#425563",
          marker = list(fillColor = "#005EB8")
        ) %>%
        highcharter::hc_subtitle(
          useHTML = TRUE,
          text =
            "
            <span style = 'color:#005EB8; font-size: 20px'> &bull; </span> <b> <span style = font-size: 35px'> Care home </span> </b>
            <span style = 'color:#425563; font-size: 20px'> &bull; </span> <b> <span style = font-size: 35px'> Non-care home </span>
            ",
          align = "center"
        ) %>%
        highcharter::hc_chart(
          inverted = TRUE,
          marginLeft = 200
        ) %>%
        highcharter::hc_scrollbar(enabled = FALSE) %>%
        theme_nhsbsa() %>%
        highcharter::hc_xAxis(
          categories = unique(metrics_by_bnf_and_ch_flag_df()$SUB_BNF_LEVEL_NAME),
          max = 19 # it shows n + 1 = 15
        ) %>%
        highcharter::hc_yAxis(
          min = 0,
          max = max(
            max(metrics_by_bnf_and_ch_flag_df()$SDC_PPM_NON_CH, na.rm = TRUE),
            max(metrics_by_bnf_and_ch_flag_df()$SDC_PPM_CH, na.rm = TRUE)
          ),
          title = list(
            text = switch(input$metric,
              "COST" = "Drug cost (PPM)",
              "ITEMS" = "Number of prescription items (PPM)"
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
                'Care home patients aged 65 years or over: ' + '<b>' + Highcharts.numberFormat(this.point.high,1) +  '</b> <br>' +
                'Non-care home patients aged 65 years or over: ' + '<b>' + Highcharts.numberFormat(this.point.low,1) + '</b>'

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
        dplyr::arrange(desc(SDC_PPM_CH)) %>%
        head(2)

      top_non_care_home_df <- metrics_by_bnf_and_ch_flag_df() %>%
        dplyr::arrange(desc(SDC_PPM_NON_CH)) %>%
        head(1)

      tags$text(
        class = "highcharts-caption",
        tags$b(top_care_home_df[1, "SUB_BNF_LEVEL_NAME"]), "and",
        tags$b(top_care_home_df[2, "SUB_BNF_LEVEL_NAME"]), "are the most ",
        "commonly prescribed BNF", paste0(input$bnf, "s"),
        switch(input$metric,
          "COST" = "drug cost ",
          "ITEMS" = "prescription items"
        ),
        " in 2020/21, accounting for ",
        switch(input$metric,
          "COST" = "£",
          "ITEMS" = ""
        ),
        paste0(top_care_home_df[1, "SDC_PPM_CH"]), " and ",
        switch(input$metric,
          "COST" = "£",
          "ITEMS" = ""
        ),
        paste0(top_care_home_df[1, "SDC_PPM_NON_CH"]), " of all ",
        switch(input$metric,
          "COST" = "drug cost to",
          "ITEMS" = "prescription items to"
        ),
        " care home patients aged 65 years or over per patient month (PPM). For non-care home patients it is",
        tags$b(top_non_care_home_df$SUB_BNF_LEVEL_NAME),
        switch(input$metric,
          "COST" = "(£ ",
          "ITEMS" = "("
        ),
        paste0(top_non_care_home_df$SDC_PPM_NON_CH, ")."),
        sep = ""
      )
    })
  })
}

## To be copied in the UI
# mod_04_commonly_prescribed_medicines_ui("04_commonly_prescribed_medicines_ui_1")

## To be copied in the server
# mod_04_commonly_prescribed_medicines_server("04_commonly_prescribed_medicines_ui_1")
