#' 04_commonly_prescribed_medicine UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_04_commonly_prescribed_medicine_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Commonly prescribed medicines"),
    fluidRow(
      col_4(
        br(),
        br(),
        br(),
        br(),
        shiny::htmlOutput(ns("text"))
      ),
      col_8(
        fluidRow(
          style = "background-color: #FFFFFF;",
          align = "center",
          h6("Medicines prescribed to older care home patients in England (2020/21)"),
          # style = "background-color: #FFFFFF;",
          col_6(
            selectInput(
              inputId = ns("bnf"),
              label = "BNF",
              choices = names(careHomePrescribingScrollytellR::bnf),
              width = "100%"
            )
          ),
          col_6(
            selectInput(
              inputId = ns("metric"),
              label = "Metric",
              choices = c(
                "Items" = "ITEMS",
                "Average drug cost" = "DRUGS",
                "Patient level" = "PATIENTS"
              ),
              width = "100%"
            )
          ),
          highcharter::highchartOutput(
            outputId = ns("items_and_cost_per_bnf_chart"),
            height = "600px",
            width = "100%"
          )
        ),
        mod_download_ui(
          id = ns("download_items_and_cost_per_bnf_chart")
        )
      )
    )





    # p("The top BNF chapters for older care home prescribing in 2020/21 are: "),
    # p("By number of prescription items: "),
    # tags$ul(
    #   tags$li(
    #     style = "font-size: 16pt;",
    #     "04-Central Nervous System"
    #   ),
    #   tags$li(
    #     style = "font-size: 16pt;",
    #     "02-Cardiovascular System"
    #   ),
    #   tags$li(
    #     style = "font-size: 16pt;",
    #     "01-Gastro-intestinal System"
    #   )
    # ),
    # p("And by drug cost:"),
    # tags$ul(
    #   tags$li(
    #     style = "font-size: 16pt;",
    #     "04-Central Nervous System"
    #   ),
    #   tags$li(
    #     style = "font-size: 16pt;",
    #     "09-Nutrition and Blood"
    #   ),
    #   tags$li(
    #     style = "font-size: 16pt;",
    #     "02-Cardiovascular System"
    #   )
    # ),
    # br(),
    #   ,
    #   p(
    #     "And ", tags$b("Paracetamol"), " (painkiller) is the most commonly prescribed ",
    #     "drug at chemical substance level by number of prescription items, accounting ",
    #     "for 5% of all prescription items to older care home patients. ",
    #     "Whereas it is ", tags$b("Atorvastatin"), "(used to lower cholesterol) in ",
    #     "older non care home patients."
    #   ),
    #   p(
    #     tags$b("Enteral nutrition"), " products however account for the greatest ",
    #     "percentage of drug cost at chemical substance level (13%) in older care home ",
    #     "patients. Whereas it is ", tags$b("Apixiban"), "(blood thinner) in older ",
    #     "non care home patients (7% of drug cost), the second most common medicine by ",
    #     "cost for care home patients."
    #   ),
    #   br(),
    #   br(),
    #   h6("Medicines prescribed to older care home patients in England (2020/21)"),
    #   fluidRow(
    #     align = "center",
    #     style = "background-color: #FFFFFF;",
    #     col_6(
    #       style = "margin-bottom: 0;",
    #       div(
    #         selectInput(
    #           inputId = ns("bnf"),
    #           label = "BNF",
    #           choices = names(careHomePrescribingScrollytellR::bnf),
    #           width = "80%"
    #         )
    #       )
    #     ),
    #     col_6(
    #       style = "margin-bottom: 0;",
    #       div(
    #         selectInput(
    #           inputId = ns("metric"),
    #           label = "Metric",
    #           choices = c("Items" = "ITEMS",
    #                       "Average drug cost" = "DRUGS",
    #                       "Patient level" = "PATIENTS"
    #                       ),
    #           width = "80%"
    #         )
    #       )
    #     ),
    #     highcharter::highchartOutput(
    #       outputId = ns("items_and_cost_per_bnf_chart"),
    #       height = "600px",
    #       width = "800px"
    #     )
    #   ),
    #   mod_download_ui(
    #     id = ns("download_items_and_cost_per_bnf_chart")
    #   )
  )
}

#' 04_commonly_prescribed_medicine Server Functions
#'
#' @noRd
mod_04_commonly_prescribed_medicine_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Process for the dumbbell chart
    items_and_cost_per_bnf_df <- reactive({
      req(input$metric)
      req(input$bnf)

      plot_df <- careHomePrescribingScrollytellR::items_and_cost_per_bnf_df %>%
        dplyr::filter(METRIC == input$metric &
          BREAKDOWN == input$bnf)

      plot_df_20 <- careHomePrescribingScrollytellR::items_and_cost_per_bnf_df %>%
        dplyr::filter(METRIC == input$metric &
          BREAKDOWN == input$bnf) %>%
        dplyr::group_by(METRIC) %>%
        dplyr::slice_max(order_by = SDC_PCT_NON_CH, n = 20) %>%
        dplyr::ungroup() %>%
        dplyr::select(METRIC, BREAKDOWN, BNF_NAME)


      plot_df <- plot_df %>%
        dplyr::inner_join(y = plot_df_20)
    })


    # Download df
    items_and_cost_per_bnf_download_df <- reactive({
      req(input$metric)
      req(input$bnf)

      plot_df <- careHomePrescribingScrollytellR::items_and_cost_per_bnf_df %>%
        dplyr::filter(METRIC == input$metric &
          BREAKDOWN == input$bnf)
    })

    # Add a download button
    mod_download_server(
      id = "download_items_and_cost_per_bnf_chart",
      filename = "items_and_cost_per_bnf_df.csv",
      export_data = items_and_cost_per_bnf_download_df()
    )

    # Need to work on it
    output$items_and_cost_per_bnf_chart <- highcharter::renderHighchart({
      req(input$metric)
      req(input$bnf)


      title <- ifelse(input$metric == "DRUGS", "drug cost", "prescription items")
      axis_title <- ifelse(input$metric == "DRUGS", "Drug cost as a % of aveerage drug cost per patient group", "Number of items as a % of all items per patient group") # totally different title so keep it like this..

      highcharter::highchart() %>%
        highcharter::hc_add_series(
          data = items_and_cost_per_bnf_df(),
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
            <span style = 'color:#005EB8; font-size: 20px'> &bull; </span> <b> <span style = font-size: 35px'> older care home patients </span> </b>
            <span style = 'color:#768692; font-size: 20px'> &bull; </span> <b> <span style = font-size: 35px'> older non-care home patients </span>
            ",
          align = "center"
        ) %>%
        highcharter::hc_chart(inverted = TRUE) %>%
        theme_nhsbsa() %>%
        highcharter::hc_title(
          text = glue::glue("Top 20 medicines by % of {title} per patient group"),
          align = "left"
        ) %>%
        highcharter::hc_xAxis(
          categories = unique(items_and_cost_per_bnf_df()$BNF_NAME),
          style = list(
            fontSize = 15
          ),
          title = list(text = unique(items_and_cost_per_bnf_df()$BREAKDOWN))
        ) %>%
        highcharter::hc_yAxis(
          labels = list(
            format = "{value}%"
          ),
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
                '<b>' + this.point.BNF_NAME + '</b> <br>' +
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

      text <- switch(input$bnf,
        "BNF Chapter" = p(
          "Around one in four items (24%) prescribed to care home patients are ",
          "from the", tags$b("Central Nervous System "),
          tippy(
            text = "BNF",
            tooltip = tooltip_text$bnf_code
          ),
          "chapter. This chapter also accounts for 24% of drug cost."
        ),
        "BNF Section" = p(
          tags$b("Analgesics "),
          "is the most common BNF section within the Central Nervous System ",
          "for care home patients in terms items and drug cost."
        ),
        "BNF Paragraph" = p("We can add key point around para?"),
        "BNF Chemical Substances" = p(
          tags$b("Paracetamol"), " (painkiller) is the most commonly prescribed ",
          "drug at chemical substance level by number of prescription items, accounting ",
          "for 5% of all prescription items to older care home patients. ",
          "Whereas it is ", tags$b("Atorvastatin"), "(used to lower cholesterol) in ",
          "older non care home patients."
        )
      )
      shiny::HTML(paste(text))
    })
  })
}

## To be copied in the UI
# mod_04_commonly_prescribed_medicine_ui("04_commonly_prescribed_medicine_ui_1")

## To be copied in the server
# mod_04_commonly_prescribed_medicine_server("04_commonly_prescribed_medicine_ui_1")
