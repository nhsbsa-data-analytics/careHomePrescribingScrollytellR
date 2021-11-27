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
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;",
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
mod_items_and_cost_per_bnf_chapter_and_section_chart_server <- function(
  input, 
  output, 
  session
) {
  ns <- session$ns

  # Hardcode metric for now
  metric <- "COST"
  
  # Limit based on metric
  items_and_cost_per_bnf_chapter_and_section_df <- 
    careHomePrescribingScrollytellR::items_and_cost_per_bnf_chapter_and_section_df %>%
    dplyr::filter(METRIC == metric)
  
  # Format data for highcharter
  plot_df <- 
    dplyr::bind_rows(
    
      # BNF chapter
      items_and_cost_per_bnf_chapter_and_section_df %>%
        dplyr::distinct(BNF_CHAPTER, PCT_LEVEL_1) %>%
        dplyr::mutate(
          id = tolower(BNF_CHAPTER),
          value = PCT_LEVEL_1
        ) %>%
        dplyr::select(name = BNF_CHAPTER, id, value) %>%
        cbind(color = nhsbsaR::palette_nhsbsa()),
      
      # BNF section
      items_and_cost_per_bnf_chapter_and_section_df %>%
        dplyr::mutate(parent = tolower(BNF_CHAPTER)) %>%
        dplyr::select(parent, name = BNF_SECTION, value = PCT_LEVEL_2) %>%
        dplyr::mutate(id = as.character(dplyr::row_number()))
      
    ) %>%
    highcharter::list_parse()

  # Pyramid plot for age band and gender
  output$items_and_cost_per_bnf_chapter_and_section_chart <- 
    highcharter::renderHighchart({
    highcharter::highchart() %>%
      highcharter::hc_chart(type = "treemap") %>%
      highcharter::hc_add_series(
        data = plot_df,
        allowDrillToNode = TRUE,
        levelIsConstant = FALSE,
        textOverflow = "clip",
        dataLabels = list(color = "white"),
        levels = list(
          list(
            level = 1,
            borderWidth = 3,
            colorByPoint = TRUE,
            # colorVariation = list(key = 'brightness', to = -0.5),
            dataLabels = list(
              enabled = TRUE,
              verticalAlign = "top",
              align = "left",
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
      highcharter::hc_title(text = "Most prescribed BNF chapter in care home prescription")
  })
}

## To be copied in the UI
# mod_items_and_cost_per_bnf_chapter_and_section_chart_ui("items_and_cost_per_bnf_chapter_and_section_chart_1")

## To be copied in the server
# callModule(mod_items_and_cost_per_bnf_chapter_and_section_chart_server, "items_and_cost_per_bnf_chapter_and_section_chart_1")
