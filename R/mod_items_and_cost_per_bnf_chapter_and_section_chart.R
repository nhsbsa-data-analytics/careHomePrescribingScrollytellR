#' items_and_cost_per_bnf_chapter_and_section_chart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_items_and_cost_per_bnf_chapter_and_section_chart_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Commonly prescribed drugs"),
    p("Text will be added."),
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;",
      h6("Drugs prescribed to older care home patients in England (2020/21)"),
      radioButtons(
        inputId = ns("metric"),
        label = "Metric",
        choices = c("Items", "Cost"),
        inline = TRUE,
        width = "100%"
      ),
      highcharter::highchartOutput(
        outputId = ns("items_and_cost_per_bnf_chapter_and_section_chart"),
        height = "500px",
        width = "800px"
      )
    )
  )
}

#' items_and_cost_per_bnf_chapter_and_section_chart Server Function
#'
#' @noRd
mod_items_and_cost_per_bnf_chapter_and_section_chart_server <- function(input,
                                                                        output,
                                                                        session) {
  ns <- session$ns
  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- ","
  options(highcharter.lang = hcoptslang)

  # Had to create reactiveValues as radiobutton holds NULL

  metric_selection <- reactiveValues(v = NULL)

  observe({
    metric_selection$v <- input$metric
  })

  # create as reactive value - now it holds selected value

  input_metric <- reactive(metric_selection$v)

  # Limit based on metric
  items_and_cost_per_bnf_chapter_and_section_df <- reactive({
    careHomePrescribingScrollytellR::items_and_cost_per_bnf_chapter_and_section_df %>%
      dplyr::filter(METRIC == input_metric())
  })


  # Define the colours (tint for the second level based on the %)
  # NOTE: Not happy with this but does the job for the moment
  items_and_cost_per_bnf_chapter_and_section_df_1 <- reactive({
    tmp_df <- items_and_cost_per_bnf_chapter_and_section_df() %>%
      dplyr::left_join(
        y = items_and_cost_per_bnf_chapter_and_section_df() %>%
          dplyr::distinct(BNF_CHAPTER) %>%
          cbind(COLOUR_LEVEL_1 = nhsbsaR::palette_nhsbsa())
      ) %>%
      dplyr::mutate(
        PRP_LEVEL_2 = ifelse(is.na(PRP_LEVEL_2), 0, PRP_LEVEL_2),
      )

    tmp_df$COLOUR_LEVEL_2 <- Vectorize(tinter::lighten)(
      tmp_df$COLOUR_LEVEL_1,
      tmp_df$PRP_LEVEL_2
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
    dplyr::bind_rows(

      # BNF chapter
      items_and_cost_per_bnf_chapter_and_section_df_1() %>%
        dplyr::distinct(BNF_CHAPTER, PRP_LEVEL_1, COLOUR_LEVEL_1, TOTAL_LEVEL_1) %>%
        dplyr::mutate(
          id = tolower(BNF_CHAPTER),
          PRP_LEVEL_1_p = PRP_LEVEL_1 * 100
        ) %>%
        dplyr::select(
          name = BNF_CHAPTER,
          id,
          value = PRP_LEVEL_1,
          p = PRP_LEVEL_1_p,
          value1 = TOTAL_LEVEL_1,
          color = COLOUR_LEVEL_1
        ),

      # BNF section
      items_and_cost_per_bnf_chapter_and_section_df_1() %>%
        dplyr::mutate(
          parent = tolower(BNF_CHAPTER),
          id = tolower(BNF_SECTION),
          PRP_LEVEL_2_p = PRP_LEVEL_2 * 100
        ) %>%
        dplyr::select(
          parent,
          name = BNF_SECTION,
          id,
          value = PRP_LEVEL_2,
          p = PRP_LEVEL_2_p,
          value1 = TOTAL_LEVEL_2,
          color = COLOUR_LEVEL_2
        )
    ) %>%
      highcharter::list_parse()
  })

  observe({
    print(plot_df())
  })


  output$items_and_cost_per_bnf_chapter_and_section_chart <- highcharter::renderHighchart({
    req(input$metric)
    # NOTE: Not sure how to deal with if statemetn with tooltip so here I am splitting two ways....
    if (input$metric == "Items") {
      highcharter::highchart() %>%
        highcharter::hc_chart(type = "treemap") %>%
        highcharter::hc_add_series(
          data = plot_df(),
          allowDrillToNode = TRUE,
          levelIsConstant = FALSE,
          textOverflow = "clip",
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
        highcharter::hc_tooltip(pointFormat = "<b> % of total items:</b> {point.p: .1f}% <br> <b> Number of items: </b> {point.value1:,.0f}")
    } else {
      highcharter::highchart() %>%
        highcharter::hc_chart(type = "treemap") %>%
        highcharter::hc_add_series(
          data = plot_df(),
          allowDrillToNode = TRUE,
          levelIsConstant = FALSE,
          textOverflow = "clip",
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
        highcharter::hc_tooltip(pointFormat = "<b> % of total NIC (£):</b> {point.p: .1f}% <br> <b> NIC (£): </b>£ {point.value1:,.0f}")
    }
  })
}

## To be copied in the UI
# mod_items_and_cost_per_bnf_chapter_and_section_chart_ui("items_and_cost_per_bnf_chapter_and_section_chart_1")

## To be copied in the server
# callModule(mod_items_and_cost_per_bnf_chapter_and_section_chart_server, "items_and_cost_per_bnf_chapter_and_section_chart_1")
