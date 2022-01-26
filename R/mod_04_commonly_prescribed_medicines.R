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
    fluidRow(
      col_4(
        shiny::htmlOutput(ns("text"))
      ),
      col_8(
        fluidRow(
          style = "background-color: #FFFFFF;",
          align = "center",
          h6(
            "Medicines prescribed to older care home patients in England ",
            "(2020/21)"
          ),
          col_6(
            selectInput(
              inputId = ns("bnf"),
              label = "BNF Level",
              choices = names(careHomePrescribingScrollytellR::bnfs),
              width = "100%"
            )
          ),
          col_6(
            selectInput(
              inputId = ns("metric"),
              label = "Metric",
              choices = c(
                "Average drug cost" = "COST",
                "Average prescription items" = "ITEMS",
                "Patient count" = "PATIENTS"
              ),
              width = "100%"
            )
          ),
          highcharter::highchartOutput(
            outputId = ns("metrics_by_bnf_and_ch_flag_chart"),
            height = "600px"
          )
        ),
        mod_download_ui(
          id = ns("download_metrics_by_bnf_and_ch_flag_chart")
        )
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

    # Filter data to BNF level and metric and take the top 20
    metrics_by_bnf_and_ch_flag_df <- reactive({
      req(input$bnf)
      req(input$metric)

      careHomePrescribingScrollytellR::metrics_by_bnf_and_ch_flag_df %>%
        dplyr::filter(
          BNF_LEVEL == input$bnf,
          METRIC == input$metric
        ) %>%
        dplyr::arrange(desc(PCT_CH)) %>%
        head(20) %>%
        dplyr::select(-c(PCT_CH, PCT_NON_CH))
    })

    # Swap NAs for "c" for data download
    metrics_by_bnf_and_ch_flag_download_df <- reactive({
      req(input$bnf)
      req(input$metric)

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

    # Need to work on it
    output$metrics_by_bnf_and_ch_flag_chart <- highcharter::renderHighchart({
      req(input$bnf)
      req(input$metric)

      # Define the axis title
      axis_title <- switch(input$metric,
        "COST" = "Drug cost as a % of average drug cost per patient group",
        "ITEMS" = "Number of items as a % of all items per patient group",
        "PATIENTS" =
          "Number of unique patients as a % of all patients per patient group"
      )

      # Create the chart
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
        highcharter::hc_chart(inverted = TRUE) %>%
        theme_nhsbsa() %>%
        highcharter::hc_xAxis(
          categories = unique(metrics_by_bnf_and_ch_flag_df()$SUB_BNF_LEVEL_NAME),
          style = list(
            fontSize = 15
          ),
          title = list(text = paste("BNF", input$bnf))
        ) %>%
        highcharter::hc_yAxis(
          labels = list(format = "{value}%"),
          min = 0,
          title = list(text = axis_title)
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

    output$text <- shiny::renderUI({
      req(input$bnf)
      req(input$metric)

      text <- dplyr::case_when(
        input$bnf == "Chapter" & input$metric == "ITEMS" ~
        paste(p(
          "Around one in four prescription items (24%) prescribed to care home ",
          "patients in 2020/21 are ",
          "from the", tags$b("central nervous system "),
          tippy(
            text = "BNF",
            tooltip = tooltip_text$bnf_code
          ),
          "chapter. This compares to 13% for older non-care home patients where ",
          "the", tags$b("cardiovascular system"), " is the most common BNF chapter by ",
          "number of prescription items."
        )),
        input$bnf == "Chapter" & input$metric == "COST" ~
        paste(p(
          "The ", tags$b("central nervous system "),
          tippy(
            text = "BNF",
            tooltip = tooltip_text$bnf_code
          ),
          " chapter also accounts for ",
          "24% of drug cost for older care home patients, compared to 12% for ",
          "older non-care home patients where again the ", tags$b("cardiovascular system "),
          "is the most common BNF chapter."
        )),
        input$bnf == "Chapter" & input$metric == "PATIENTS" ~
        paste(p(
          "Around 9 in 10 (91%) older care home patients received at last one ",
          "prescription item from the ", tags$b("central nervous system "),
          "during 2020/21 compared to half of (49%) older non-care home patients. ",
          "As with prescription items, the ", tags$b("cardiovascular system "),
          "is the most common BNF chapter in terms of number of older non-care",
          " home patients receiving at least one prescription item (78%)"
        )),
        input$bnf == "Section" & input$metric == "ITEMS" ~
        paste(p(
          tags$b("Analgesics "), "(painkillers) and ", tags$b("laxatives "),
          "are the most common BNF sections for older care home patients in ",
          "2020/21, accounting for 8% and 7% in terms of prescription items respectively. ",
          "This compares to 5% and 2% for older non-care home patients where ",
          tags$b("lipid-regulating drugs "), "(for raised cholesterol) is the most ",
          "common BNF section (9%)."
        )),
        input$bnf == "Section" & input$metric == "COST" ~
        paste(p(
          tags$b("Oral nutrition "), "(nutrition supplement) products however ",
          "account for the greatest percentage of drug cost at ",
          tippy(
            text = "BNF",
            tooltip = tooltip_text$bnf_code
          ),
          " section level in ",
          "older care home patients (14% compared to 2% in older non-care home patients).",
          "Whereas it is ", tags$b("anticoagulants and protamine "),
          "(counteract the anticoagulant effect)",
          " in older non-care home patients (14% of drug cost) compared to 11% ",
          "for older care home patients."
        )),
        input$bnf == "Section" & input$metric == "PATIENTS" ~
        paste(p(
          tags$b("Analgesics "), "(painkillers) and ", tags$b("laxatives "),
          "are the most common BNF sections for older care home patients. ",
          "There were 76% and 61% of patients respectively receiving at least ",
          "one prescription item. As with prescription items, ",
          tags$b("lipid regulating drugs "), "is the most common BNF section ",
          "in terms of the number of older non-care home patients receivng at ",
          "least one prescription item (54%)."
        )),
        input$bnf == "Paragraph" & input$metric == "ITEMS" ~
        paste(p(
          tags$b("Non-opioid analgesics and compound preparations "), "(to relieve pain) ",
          "and ", tags$b("proton pump inhibitors "), "(acid reflux) each account for ",
          "5% of prescription items for older care home patients in 2020/21. This ",
          "compares to 3% for older non-care home patients where around one in ",
          "ten prescription items (9%) are for ",
          tags$b("lipid-regulating drugs "), "(raised cholesterol)."
        )),
        input$bnf == "Paragraph" & input$metric == "COST" ~
        paste(p(
          tags$b("Enteral nutrition "), "(enteral feeding) ",
          "products however account for the greatest percentage of drug cost ",
          "at ",
          tippy(
            text = "BNF",
            tooltip = tooltip_text$bnf_code
          ),
          " paragraph level (13%) in older care home patients. ",
          "Whereas it is ", tags$b("oral anticoagulants "), "(prevent blood clots) ",
          "in older non-care home patients (13% of drug cost), the second most ",
          "common medicine by drug cost for older care home patients."
        )),
        input$bnf == "Paragraph" & input$metric == "PATIENTS" ~
        paste(p(
          tags$b("Non-opiod analgesics and compound preparations "),
          "(to relieve pain) is the most common ",
          tippy(
            text = "BNF",
            tooltip = tooltip_text$bnf_code
          ),
          " section, ",
          "with 68% of older care home patients ",
          "receiving at least one prescriptoin item. ",
          tags$b("Lipid regulating drugs "),
          "is the most common BNF section for ",
          "non-care home patients with 54% receiving at least one ",
          "prescription item, ",
          "compared to 31% of older care home patients."
        )),
        input$bnf == "Chemical Substance" & input$metric == "ITEMS" ~
        paste(p(
          tags$b("Paracetamol "), "(painkiller) and ",
          tags$b("Colecalciferol "),
          "(vitamin D) are the most commonly ",
          "prescribed drugs at chemical substance ",
          "level by number of prescription items in 2020/21, ",
          "accounting for 5% and ",
          "4% of all prescription items to older care home patients.",
          " Whereas it is ", tags$b("Atorvastatin "),
          "(used to lower cholesterol) in older non-care ",
          "home patients (6%)."
        )),
        input$bnf == "Chemical Substance" & input$metric == "COST" ~
        paste(p(
          tags$b("Enteral nutrition "),
          "products however account for the greatest percentage ",
          "of drug cost at chemical substance level (13%) ",
          "in older care home patients. Whereas ",
          "it is ", tags$b("Apixaban "),
          "(blood thinner) in older non-care home patients ",
          "(7% of drug cost), ",
          "the second most common medicine by drug cost for care home patients."
        )),
        input$bnf == "Chemical Substance" & input$metric == "PATIENTS" ~
        paste(p(
          tags$b("Paracetamol "),
          "(painkiller) is the most commonly prescribed, with 64% of older care ",
          "home patients receiving at least once prescription item.",
          " This compares to 16% of older non-care home patients. ",
          "As with prescription items, ", tags$b("Atovastatin "),
          "(used to lower cholesterol) is the most common medicine, ",
          "based on the number of older non-care home patients receiving ",
          "at least one prescription item."
        ))
      )

      shiny::HTML(paste(text))
    })
  })
}

## To be copied in the UI
# mod_04_commonly_prescribed_medicines_ui("04_commonly_prescribed_medicines_ui_1")

## To be copied in the server
# mod_04_commonly_prescribed_medicines_server("04_commonly_prescribed_medicines_ui_1")
