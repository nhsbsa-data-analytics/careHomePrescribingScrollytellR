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
        "The profile of medicines prescribed to care home patients aged 65 years or over",
        "differs significantly to non-care home patients aged 65 years or over."
      )
    ),
    p(
      "Care home patients are more likely to receive",
      tags$b("drugs for pain relief"), "than non-care home patients. ",
      "Whilst", tags$b("nutrition products"), "account for a greater ",
      "percentage of drug cost."
    ),
    p(
      "The chart below allows you to view and compare the 20 most common ",
      "medicines prescribed to older care home patients and older non-care ",
      "home patients by drug cost and number of prescription items per patient ",
      "month and annually."
    ),
    p(
      "Medicines have been identified using a",
      tippy(
        text = "BNF code and are grouped at 4 levels.",
        tooltip = tooltip_text$bnf_code
      )
    ),
    nhs_card(
      heading = "Estimated average prescribing metrics per patient month for care home and non-care home patients aged 65 years or over in England by medicine group (2020/21)",
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
            "Number of prescription items (PPM)" = "ITEMS",
            "% of total annual drug cost" = "COST_PERC",
            "% of total annual number of prescription items" = "ITEMS_PERC"
          ),
          full_width = TRUE
        ),
        nhs_selectInput(
          inputId = ns("sort"),
          label = "Sort by",
          choices = c(
            "Care home" = "SDC_CH_VALUE",
            "Non-care home" = "SDC_NON_CH_VALUE"
          ),
          full_width = TRUE
        )
      ),
      shiny::uiOutput(outputId = ns("text")),
      highcharter::highchartOutput(
        outputId = ns("metrics_by_bnf_and_ch_flag_chart"),
        height = "500px"
      ),
      mod_nhs_download_ui(
        id = ns("download_metrics_by_bnf_and_ch_flag_chart") # need to update and fix
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


    # Need to join PPM data and Percentage data as now we want to show 4 metrics

    # Filter data to BNF level, metric, and sort (taking the top 20 for percentage)
    metrics_by_bnf_and_ch_flag_df <- reactive({
      req(input$bnf)
      req(input$metric)
      req(input$sort)

      if (input$metric == "ITEMS_PERC") {
        careHomePrescribingScrollytellR::metrics_by_bnf_and_ch_flag_perc_df %>%
          dplyr::filter(
            BNF_LEVEL == input$bnf,
            METRIC == input$metric
          ) %>%
          dplyr::arrange(desc(.data[[input$sort]])) %>%
          head(20) %>%
          dplyr::select(-c(CH_VALUE, NON_CH_VALUE))
      } else if (input$metric == "COST_PERC") {
        careHomePrescribingScrollytellR::metrics_by_bnf_and_ch_flag_perc_df %>%
          dplyr::filter(
            BNF_LEVEL == input$bnf,
            METRIC == input$metric
          ) %>%
          dplyr::arrange(desc(.data[[input$sort]])) %>%
          head(20) %>%
          dplyr::select(-c(CH_VALUE, NON_CH_VALUE))
      } else if (input$metric == "ITEMS") {
        careHomePrescribingScrollytellR::metrics_by_bnf_and_ch_flag_df %>%
          dplyr::filter(
            BNF_LEVEL == input$bnf,
            METRIC == input$metric
          ) %>%
          dplyr::arrange(desc(.data[[input$sort]])) %>%
          dplyr::select(-c(CH_VALUE, NON_CH_VALUE))
      } else {
        careHomePrescribingScrollytellR::metrics_by_bnf_and_ch_flag_df %>%
          dplyr::filter(
            BNF_LEVEL == input$bnf,
            METRIC == input$metric
          ) %>%
          dplyr::arrange(desc(.data[[input$sort]])) %>%
          dplyr::select(-c(CH_VALUE, NON_CH_VALUE))
      }
    })


    # Swap NAs for "c" for data download
    metrics_by_bnf_and_ch_flag_download_df <- reactive({
      req(input$bnf)
      req(input$metric)
      req(input$sort)

      metrics_by_bnf_and_ch_flag_df <- metrics_by_bnf_and_ch_flag_df() %>%
        dplyr::mutate(
          SDC_CH_VALUE = janitor::round_half_up(SDC_CH_VALUE, 1),
          SDC_NON_CH_VALUE = janitor::round_half_up(SDC_NON_CH_VALUE, 1)
        ) %>%
        dplyr::mutate(SUB_BNF_LEVEL_NAME = gsub(",", "", SUB_BNF_LEVEL_NAME))


      if (input$metric == "ITEMS_PERC" | input$metric == "COST_PERC") {
        metrics_by_bnf_and_ch_flag_df %>%
          dplyr::select(
            BNF_LEVEL,
            SUB_BNF_LEVEL_NAME,
            METRIC,
            SDC_CH_VALUE,
            TOTAL_CH,
            SDC_NON_CH_VALUE,
            TOTAL_NON_CH
          ) %>%
          dplyr::mutate(
            TOTAL_CH = round(TOTAL_CH, -1),
            TOTAL_NON_CH = round(TOTAL_NON_CH, -1)
          ) %>%
          dplyr::rename(
            `BNF` = BNF_LEVEL,
            `BNF description` = SUB_BNF_LEVEL_NAME,
            Metric = METRIC,
            `Care home percentage` = SDC_CH_VALUE,
            `Care home total` = TOTAL_CH,
            `Non care home percentage` = SDC_NON_CH_VALUE,
            `Non Care home total` = TOTAL_NON_CH
          )
      } else {
        metrics_by_bnf_and_ch_flag_df %>%
          dplyr::select(
            BNF_LEVEL,
            SUB_BNF_LEVEL_NAME,
            METRIC,
            SDC_CH_VALUE,
            SDC_NON_CH_VALUE
          ) %>%
          dplyr::rename(
            `BNF` = BNF_LEVEL,
            `BNF description` = SUB_BNF_LEVEL_NAME,
            Metric = METRIC,
            `Care home ppm` = SDC_CH_VALUE,
            `Non care home ppm` = SDC_NON_CH_VALUE,
          )
      }
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

      # filter data depends on

      highcharter::highchart() %>%
        highcharter::hc_add_series(
          data = metrics_by_bnf_and_ch_flag_df(),
          type = "dumbbell",
          highcharter::hcaes(
            low = SDC_NON_CH_VALUE,
            high = SDC_CH_VALUE
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
            max(metrics_by_bnf_and_ch_flag_df()$SDC_NON_CH_VALUE, na.rm = TRUE),
            max(metrics_by_bnf_and_ch_flag_df()$SDC_CH_VALUE, na.rm = TRUE)
          ),
          title = list(
            text = switch(input$metric,
              "COST" = "Drug cost (PPM)",
              "ITEMS" = "Number of prescription items (PPM)",
              "COST_PERC" = "% of total annual drug cost",
              "ITEMS_PERC" = "% of total annual number of prescription items"
            )
          )
        ) %>%
        highcharter::hc_legend(enabled = FALSE) %>%
        highcharter::hc_tooltip(
          useHTML = TRUE,
          formatter = htmlwidgets::JS(
            "
            function() {
              if(this.point.METRIC == 'ITEMS_PERC' | this.point.METRIC == 'COST_PERC'){
                outHTML =
                  '<b>' + this.point.SUB_BNF_LEVEL_NAME + '</b> <br>' +
                  'Care home patients aged 65 years or over: ' + '<b>' + Highcharts.numberFormat(this.point.high,1) +  '% </b> <br>' +
                  'Non-care home patients aged 65 years or over: ' + '<b>' + Highcharts.numberFormat(this.point.low,1) + '% </b>'
              }else{
                outHTML =
                  '<b>' + this.point.SUB_BNF_LEVEL_NAME + '</b> <br>' +
                  'Care home patients aged 65 years or over: ' + '<b>' + Highcharts.numberFormat(this.point.high,1) +  '</b> <br>' +
                  'Non-care home patients aged 65 years or over: ' + '<b>' + Highcharts.numberFormat(this.point.low,1) + '</b>'
              }
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
        dplyr::arrange(desc(SDC_CH_VALUE)) %>%
        head(2)

      top_non_care_home_df <- metrics_by_bnf_and_ch_flag_df() %>%
        dplyr::arrange(desc(SDC_NON_CH_VALUE)) %>%
        head(1)

      tags$text(
        class = "highcharts-caption",
        # Chapter and selected metrics
        if (input$bnf == "Chapter" & input$metric == "COST") {
          HTML(paste(
            "The highest drug costs per patient month are in the ",
            tags$b(top_care_home_df[1, "SUB_BNF_LEVEL_NAME"]), " and ",
            tags$b(top_care_home_df[2, "SUB_BNF_LEVEL_NAME"]), " BNF Chapters, ",
            " among care home patients aged 65 years or over. The prescribing ",
            "rates are around four times and seven times higher respectively ",
            "than for non-care home patients. For non-care home patients aged ",
            "65 years or over the highest drug cost per patient month is in ",
            "the ", tags$b(top_non_care_home_df$SUB_BNF_LEVEL_NAME), ".",
            sep = ""
          ))
        } else if (input$bnf == "Chapter" & input$metric == "ITEMS") {
          HTML(paste(
            "For care home patients aged 65 years or over the ",
            tags$b(top_care_home_df[1, "SUB_BNF_LEVEL_NAME"]), " BNF Chapter ",
            "also represents the highest number of prescription items ",
            "per patient month. This is followed by the ",
            tags$b(top_care_home_df[2, "SUB_BNF_LEVEL_NAME"]), ", which has the ",
            "highest number of items per patient month for non-care homes ",
            "patients aged 65 years or over.",
            sep = ""
          ))
        } else if (input$bnf == "Section" & input$metric == "COST") {
          HTML(paste(
            "The highest drug cost per patient month for care home patients ",
            "aged 65 years or over is in the ",
            tags$b(top_care_home_df[1, "SUB_BNF_LEVEL_NAME"]),
            " BNF Section. This is followed by ",
            tags$b(top_non_care_home_df$SUB_BNF_LEVEL_NAME), ", which has the ",
            "highest drug cost per patient month for non-care home patients.",
            sep = ""
          ))
        } else if (input$bnf == "Section" & input$metric == "ITEMS") {
          HTML(paste(
            tags$b(top_care_home_df[1, "SUB_BNF_LEVEL_NAME"]), " and ",
            tags$b(top_care_home_df[2, "SUB_BNF_LEVEL_NAME"]), " BNF Sections ",
            "have the highest number of prescription items per patient for care ",
            "home patients aged 65 years or over. For non-care home patients, ",
            "the highest number of prescription items per patients is for ",
            tags$b(top_non_care_home_df$SUB_BNF_LEVEL_NAME), ".",
            sep = ""
          ))
        } else if (input$bnf == "Paragraph" & input$metric == "COST") {
          tags$text(
            tags$b(top_care_home_df[1, "SUB_BNF_LEVEL_NAME"]), " and ",
            tags$b(top_care_home_df[2, "SUB_BNF_LEVEL_NAME"]), " have by far ",
            "the highest drug cost per patient month for care home patients ",
            "aged 65 years or over. For non-care home patients it is highest in ",
            "the ", tags$b(top_non_care_home_df$SUB_BNF_LEVEL_NAME), " BNF Paragraph."
          )
        } else if (input$bnf == "Paragraph" & input$metric == "ITEMS") {
          HTML(paste(
            "At BNF Paragraph level, ",
            tags$b(top_care_home_df[1, "SUB_BNF_LEVEL_NAME"]), " and ",
            tags$b(top_care_home_df[2, "SUB_BNF_LEVEL_NAME"]), " have the highest ",
            "number of prescription items per patient for care home patients aged ",
            "65 years or over. For non-care home patients the rate is highest for ",
            tags$b(top_non_care_home_df$SUB_BNF_LEVEL_NAME), ".",
            sep = ""
          ))
        } else if (input$bnf == "Chemical Substance" & input$metric == "COST") {
          tags$text(
            tags$b(top_care_home_df[1, "SUB_BNF_LEVEL_NAME"]), " has by far the ",
            "highest drug cost per patient month for care home patients aged ",
            "65 years or over. For non-care home patients it is ",
            tags$b(top_non_care_home_df$SUB_BNF_LEVEL_NAME), " at Chemical ",
            "Substance level."
          )
        } else if (input$bnf == "Chemical Substance" & input$metric == "ITEMS") {
          tags$text(
            "At Chemical Substance level, ",
            tags$b(top_care_home_df[1, "SUB_BNF_LEVEL_NAME"]), " and ",
            tags$b(top_care_home_df[2, "SUB_BNF_LEVEL_NAME"]), " have the highest ",
            "number of prescription items per patient for care home patients ",
            "aged 65 years or over. For non-care home patients it is ",
            tags$b(top_non_care_home_df$SUB_BNF_LEVEL_NAME), "(0.34)"
          )
        } else {
          tags$text(
            tags$b(top_care_home_df[1, "SUB_BNF_LEVEL_NAME"]), "and",
            tags$b(top_care_home_df[2, "SUB_BNF_LEVEL_NAME"]), "are the most ",
            "commonly prescribed BNF", paste0(input$bnf, "s"), " by percentage of ",
            switch(input$metric,
              "COST_PERC" = "drug cost ",
              "ITEMS_PERC" = "prescription items"
            ),
            " in 2020/21, accounting for ",
            paste0(top_care_home_df[1, "SDC_CH_VALUE"], "%"), " and ",
            paste0(top_care_home_df[2, "SDC_CH_VALUE"], "%"), " of all ",
            switch(input$metric,
              "COST_PERC" = "drug cost to",
              "ITEMS_PERC" = "prescription items to"
            ),
            " older care home patients. For non-care home patients it is",
            tags$b(top_non_care_home_df$SUB_BNF_LEVEL_NAME),
            paste0("(", top_non_care_home_df$SDC_NON_CH_VALUE, "%).")
          )
        }
      )
    })
  })
}

## To be copied in the UI
# mod_04_commonly_prescribed_medicines_ui("04_commonly_prescribed_medicines_ui_1")

## To be copied in the server
# mod_04_commonly_prescribed_medicines_server("04_commonly_prescribed_medicines_ui_1")
