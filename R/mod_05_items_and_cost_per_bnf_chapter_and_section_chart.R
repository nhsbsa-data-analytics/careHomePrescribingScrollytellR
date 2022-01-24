#' 05_items_and_cost_per_bnf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_05_items_and_cost_per_bnf_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Commonly prescribed medicines"),
    p(
      "Around one in four items (24%) prescribed to care home patients are ",
      "from the Central Nervous System",
      tippy(
        text = "BNF",
        tooltip = tooltip_text$bnf_code
      ),
      "chapter. This chapter also accounts for 24% of drug cost. Analgesics ",
      "is the most common BNF section within the Central Nervous System."
    ),
    br(),
    br(),
    h6("Medicines prescribed to older care home patients in England (2020/21)"),
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;",
      radioButtons(
        inputId = ns("metric"),
        label = "",
        choices = c("Items", "Drug Cost"),
        inline = TRUE,
        width = "100%"
      ),
      highcharter::highchartOutput(
        outputId = ns("items_and_cost_per_bnf_chapter_and_section_chart"),
        height = "500px",
        width = "800px"
      )
    ),
    mod_download_ui(
      id = ns("download_items_and_cost_per_bnf_chapter_and_section_chart")
    ),
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;",
      br(),
      br(),
      highcharter::highchartOutput(
        outputId = ns("items_and_cost_per_bnf_paragraph_chart"),
        height = "600px",
        width = "800px"
      )
    ),
    mod_download_ui(
      id = ns("download_items_and_cost_per_bnf_paragraph_chart")
    )
  )
}

#' 05_items_and_cost_per_bnf Server Functions
#'
#' @noRd
mod_05_items_and_cost_per_bnf_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Limit based on metric
    metric_by_bnf_and_ch_flag_df <- reactive({
      
      req(input$metric)

      careHomePrescribingScrollytellR::metrics_by_bnf_and_ch_flag_df %>%
        dplyr::filter(METRIC == input$metric)
      
    })

    # Add a download button
    mod_download_server(
      id = "download_metric_by_bnf_and_ch_flag_chart",
      filename = "metric_by_bnf_and_ch_flag_df.csv",
      export_data = metric_by_bnf_and_ch_flag_df()
    )

    # Define the colours (tint for the second level based on the %)
    # NOTE: Not happy with this but does the job for the moment
    items_and_cost_per_bnf_chapter_and_section_df_1 <- reactive({
      req(input$metric)
      tmp_df <- items_and_cost_per_bnf_chapter_and_section_df() %>%
        dplyr::left_join(
          y = items_and_cost_per_bnf_chapter_and_section_df() %>%
            dplyr::distinct(BNF_CHAPTER) %>%
            cbind(COLOUR_LEVEL_1 = nhsbsaR::palette_nhsbsa())
        ) %>%
        dplyr::mutate(
          SDC_PCT_LEVEL_2 = tidyr::replace_na(SDC_PCT_LEVEL_2, 0)
        )

      tmp_df$COLOUR_LEVEL_2 <- Vectorize(tinter::lighten)(
        tmp_df$COLOUR_LEVEL_1,
        tmp_df$SDC_PCT_LEVEL_2 / 100 / 2 + 0.2
      )

      tmp_df %>%
        dplyr::mutate(
          COLOUR_LEVEL_2 = as.character(COLOUR_LEVEL_2),
          COLOUR_LEVEL_2 = ifelse(
            test = COLOUR_LEVEL_2 == "character(0)",
            yes = "#FFFFFF",
            no = COLOUR_LEVEL_2
          )
        )
    })

    # Format data for highcharter
    plot_df <- reactive({
      req(input$metric)

      dplyr::bind_rows(
        # BNF chapter
        items_and_cost_per_bnf_chapter_and_section_df_1() %>%
          dplyr::distinct(
            BNF_CHAPTER, SDC_PCT_LEVEL_1, COLOUR_LEVEL_1, SDC_TOTAL_LEVEL_1
          ) %>%
          dplyr::mutate(
            id = tolower(BNF_CHAPTER)
          ) %>%
          dplyr::select(
            name = BNF_CHAPTER,
            id,
            value = SDC_PCT_LEVEL_1,
            value_total = SDC_TOTAL_LEVEL_1,
            color = COLOUR_LEVEL_1
          ),

        # BNF section
        items_and_cost_per_bnf_chapter_and_section_df_1() %>%
          dplyr::mutate(
            parent = tolower(BNF_CHAPTER),
            id = tolower(BNF_SECTION),
          ) %>%
          dplyr::select(
            parent,
            name = BNF_SECTION,
            id,
            value = SDC_PCT_LEVEL_2,
            value_total = SDC_TOTAL_LEVEL_2,
            color = COLOUR_LEVEL_2
          )
      ) %>%
        highcharter::list_parse()
    })


    output$items_and_cost_per_bnf_chapter_and_section_chart <- highcharter::renderHighchart({
      req(input$metric)

      # Create the shared part of the chart first
      chart <- highcharter::highchart() %>%
        highcharter::hc_chart(type = "treemap") %>%
        highcharter::hc_add_series(
          data = plot_df(),
          allowDrillToNode = TRUE,
          levelIsConstant = FALSE,
          textOverflow = "clip",
          drillUpButton = list(text = "<< Back to BNF Chapter"),
          dataLabels = list(color = "white"),
          levels = list(
            list(
              level = 1,
              borderWidth = 3,
              colorByPoint = TRUE,
              dataLabels = list(
                enabled = TRUE,
                verticalAlign = "top",
                align = "left",
                color = "black",
                style = list(fontSize = "12px", textOutline = FALSE)
              )
            ),
            list(
              level = 2,
              borderWidth = 0.2,
              dataLabels = list(enabled = FALSE)
            )
          )
        ) %>%
        theme_nhsbsa() %>%
        highcharter::hc_subtitle(
          text = "Click points to drill down to BNF Section level"
        )

      # Add the tooltip based on the metric
      if (input$metric == "Items") {
        chart %>%
          highcharter::hc_title(
            text = "Number and % of prescription items by BNF Chapter and Section",
            align = "left"
          ) %>%
          highcharter::hc_tooltip(
            useHTML = TRUE,
            formatter = htmlwidgets::JS(
              "
              function() {

                if (this.point.parent == null) {

                  outHTML =
                    '<b> % of total items: </b>' + this.point.value + '%' + '<br>'

                } else {

                  outHTML =
                    '<b> % of total items in </b>' + '<b>' + this.point.parent + '</b>'+ ': ' + this.point.value + '%' + '<br>'

                }

                outHTML = outHTML +
                  '<b> Number of items: </b>' + Highcharts.numberFormat(this.point.value_total, 0)

                return(outHTML)

              }
              "
            )
          )
      } else {
        chart %>%
          highcharter::hc_title(
            text = "Number and % of drug cost by BNF Chapter and Section",
            align = "left"
          ) %>%
          highcharter::hc_tooltip(
            useHTML = TRUE,
            formatter = htmlwidgets::JS(
              "
              function() {

                if (this.point.parent == null) {

                  outHTML =
                    '<b> % of total Net Ingredient Cost (NIC (£)): </b>' + this.point.value + '%' + '<br>'

                } else {

                  outHTML =
                    '<b> % of total NIC in </b>' + '<b>' + this.point.parent + '</b>'+ ': ' + this.point.value + '%' + '<br>'

                }

                outHTML = outHTML +
                  '<b> Total NIC (£): </b>' + '£' + Highcharts.numberFormat(this.point.value_total, 0)

                return(outHTML)

              }
            "
            )
          )
      }
    })


    # Process for the dumbbell chart
    items_and_cost_per_bnf_paragraph_df <- reactive({
      req(input$metric)

      careHomePrescribingScrollytellR::items_and_cost_per_bnf_paragraph_df %>%
        dplyr::filter(METRIC == input$metric)
    })

    # Add a download button
    mod_download_server(
      id = "download_items_and_cost_per_bnf_paragraph_chart",
      filename = "items_and_cost_per_bnf_paragraph_df.csv",
      export_data = items_and_cost_per_bnf_paragraph_df()
    )

    # Need to work on it
    output$items_and_cost_per_bnf_paragraph_chart <- highcharter::renderHighchart({
      req(input$metric)

      title <- ifelse(input$metric == "Drug Cost", "drug cost", "prescription items")
      axis_title <- ifelse(input$metric == "Drug Cost", "Drug cost as a % of total drug cost per patient group", "Number of items as a % of all items per patient group") # totally different title so keep it like this..

      highcharter::highchart() %>%
        highcharter::hc_add_series(
          data = items_and_cost_per_bnf_paragraph_df(),
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
          categories = unique(items_and_cost_per_bnf_paragraph_df()$BNF_PARAGRAPH),
          style = list(
            fontSize = 15
          ),
          title = list(text = "BNF Paragraph")
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
                '<b>' + this.point.BNF_PARAGRAPH + '</b> <br>' +
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
# mod_05_items_and_cost_per_bnf_ui("05_items_and_cost_per_bnf_ui_1")

## To be copied in the server
# mod_05_items_and_cost_per_bnf_server("05_items_and_cost_per_bnf_ui_1")
