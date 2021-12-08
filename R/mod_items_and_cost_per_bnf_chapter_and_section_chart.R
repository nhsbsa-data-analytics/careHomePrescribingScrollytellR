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
      h6("Medicines prescribed to older care home patients in England (2020/21)"),
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
      ),
      br(),
      br(),
      highcharter::highchartOutput(
        outputId = ns("items_and_cost_top_ch_para_chart"),
        height = "600px",
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
  # comma separate setting. (otherwise comma didn't show)
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
          cbind(COLOUR_LEVEL_1 = nhsbsaR::palette_nhsbsa()) # Other change to differnet colour (not significant)
      ) %>%
      dplyr::mutate(
        PRP_LEVEL_2 = ifelse(is.na(PRP_LEVEL_2), 0, PRP_LEVEL_2),
      )

    tmp_df$COLOUR_LEVEL_2 <- Vectorize(tinter::lighten)(
      tmp_df$COLOUR_LEVEL_1,
      tmp_df$PRP_LEVEL_2 / 2 + 0.2
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
          id = tolower(BNF_CHAPTER)
        ) %>%
        dplyr::select(
          name = BNF_CHAPTER,
          id,
          value = PRP_LEVEL_1,
          value_total = TOTAL_LEVEL_1,
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
          value = PRP_LEVEL_2,
          value_total = TOTAL_LEVEL_2,
          color = COLOUR_LEVEL_2
        )
    ) %>%
      highcharter::list_parse()
  })

  # observe({
  #   print(plot_df())
  # })


  output$items_and_cost_per_bnf_chapter_and_section_chart <- highcharter::renderHighchart({
    req(input$metric)
    # similar to dumbbell chart, make title to dynamic
    title <- ifelse(input$metric == "Drug Cost", "drug cost", "prescription items")

    # NOTE: Not sure how to deal with if statemetn with tooltip so here I am splitting two ways.... (It could improve using JS)
    if (input$metric == "Items") {
      highcharter::highchart() %>%
        highcharter::hc_chart(type = "treemap") %>%
        highcharter::hc_add_series(
          data = plot_df(),
          allowDrillToNode = TRUE,
          levelIsConstant = FALSE,
          textOverflow = "clip",
          drillUpButton = list(text = '<< Back BNF Chapter'),
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
        highcharter::hc_title(
          text = glue::glue("Number and % of {title} by BNF Section and Chapter"),
          align = "left"
        ) %>%
        # useful: https://www.titanwolf.org/Network/q/9ba6af5e-1a32-404f-aefb-bc9ce6daf227/y (wrap around highcharts.numberFormat)
        highcharter::hc_tooltip(
          useHTML = TRUE,
          formatter = htmlwidgets::JS(
            "
            function(){
            if (this.point.parent == null) {
            outHTML = '<b> % of total items: </b>' + (Math.round(this.point.value*100)).toFixed(1) + '%' + '<br>' + '<b> Number of items: </b>' + Highcharts.numberFormat(Math.round(this.point.value_total/ 1000) ,0) + 'K'
            return(outHTML)
            } else {
            outHTML = '<b> % of total items in </b>' + '<b>' + this.point.parent + '</b>'+ ': ' + (Math.round(this.point.value*100)).toFixed(1) + '%' + '<br>' + '<b> Number of items: </b>' + Highcharts.numberFormat(Math.round(this.point.value_total/ 1000),0) + 'K'
            return(outHTML)
            }
            }
            "
          )
        )
    } else {
      highcharter::highchart() %>%
        highcharter::hc_chart(type = "treemap") %>%
        highcharter::hc_add_series(
          data = plot_df(),
          allowDrillToNode = TRUE,
          levelIsConstant = FALSE,
          textOverflow = "clip",
          drillUpButton = list(text = '<< Back BNF Chapter'),
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
        highcharter::hc_title(
          text = glue::glue("Number and % of {title} by BNF Section and Chapter"),
          align = "left"
        ) %>%
        highcharter::hc_tooltip(
          useHTML = TRUE,
          formatter = htmlwidgets::JS(
            "
            function(){
            if (this.point.parent == null) {
            outHTML = '<b> % of total Net Ingredient Cost (NIC (£)): </b>' + (Math.round(this.point.value*100)).toFixed(1) + '%' + '<br>' + '<b> Total NIC (£): </b>' + '£' + Highcharts.numberFormat(Math.round(this.point.value_total/ 100000),0) + 'M'
            return(outHTML)
            } else {
            outHTML = '<b> % of total NIC in </b>' + '<b>' + this.point.parent + '</b>'+ ': ' + (Math.round(this.point.value*100)).toFixed(1) + '%' + '<br>' + '<b> Total NIC (£): </b>' + '£' + Highcharts.numberFormat(Math.round(this.point.value_total/100000),0) + 'M'
            return(outHTML)
            }
            }
            "
          )
        )
    }
  })


  #### Process for the dumbbell chart
  items_and_cost_top_20_df <- reactive({
    careHomePrescribingScrollytellR::top20_df %>%
      dplyr::filter(METRIC == input_metric()) %>%
      dplyr::arrange(desc(CH_P))
  })


  # Need to work on it

  output$items_and_cost_top_ch_para_chart <- highcharter::renderHighchart({
    req(input$metric)

    title <- ifelse(input$metric == "Drug Cost", "drug cost", "prescription items")
    axis_title <- ifelse(input$metric == "Drug Cost", "Drug cost as a % of total drug cost per patient group","Number of items as a % of all items per patient group") # totally different title so keep it like this..

    highcharter::highchart() %>%
      highcharter::hc_add_series(
        data = items_and_cost_top_20_df(),
        type = "dumbbell",
        highcharter::hcaes(
          low = NONE_CH_P * 100,
          high = CH_P * 100
        ),
        lowColor = "#768692",
        color = "#768692",
        marker = list(fillColor = "#005EB8")
      ) %>%
      highcharter::hc_subtitle(
        useHTML = TRUE,
        text = '<span style = "color:#005EB8; font-size: 20px"> &bull; </span> <b>
      <span style = font-size: 35px"> older care home patients </span>
      </b> <span style = "color:#768692; font-size: 20px"> &bull;
      </span> <b> <span style = font-size: 35px"> older non-care home patients </span>'
      ) %>%
      highcharter::hc_chart(inverted = TRUE) %>%
      theme_nhsbsa() %>%
      highcharter::hc_title(
        text = glue::glue("Top 20 medicines by % of {title} per patient group"),
        align = 'left'
      ) %>%
      highcharter::hc_xAxis(
        categories = unique(items_and_cost_top_20_df()$BNF_PARAGRAPH),
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
        # shared = TRUE,
        useHTML = TRUE,
        formatter = htmlwidgets::JS(
          "function(){
            outHTML = '<b>' + this.point.BNF_PARAGRAPH + '</b> <br>' + 'Older care home patients: ' + '<b>' + this.point.high.toFixed(1) + ' %' + '</b> <br>' + 'Older non-care home patients: ' + '<b>' + this.point.low.toFixed(1) + ' %' + '</b>'
            return(outHTML)
          }
         "
        )
      )
  })
}

## To be copied in the UI
# mod_items_and_cost_per_bnf_chapter_and_section_chart_ui("items_and_cost_per_bnf_chapter_and_section_chart_1")

## To be copied in the server
# callModule(mod_items_and_cost_per_bnf_chapter_and_section_chart_server, "items_and_cost_per_bnf_chapter_and_section_chart_1")
