#' bnf_ch_item_treemap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bnf_ch_item_treemap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;",
      highcharter::highchartOutput(
        outputId = ns("bnf_chapter_carehome_treemap"),
        height = "500px",
        width = "800px"
      )
    )
  )
}

#' bnf_chapter_carehome_treemap Server Function
#'
#' @noRd
mod_bnf_ch_item_treemap_server <- function(input, output, session) {
  ns <- session$ns

  cols <- NHSRtheme::get_nhs_colours(section = "highlights")

  # df1 - BNF Chapter level
  df1 <- careHomePrescribingScrollytellR::bnf_ch_final %>%
    dplyr::distinct(BNF_CHAPTER, ITEM_P_LEVEL1) %>%
    dplyr::mutate(
      id = tolower(BNF_CHAPTER),
      value = ITEM_P_LEVEL1
    ) %>%
    dplyr::select(
      name = BNF_CHAPTER,
      id,
      value
    ) %>%
    cbind(color = cols)

  # df2 - BNF Section level
  df2 <- careHomePrescribingScrollytellR::bnf_ch_final %>%
    dplyr::mutate(parent = tolower(BNF_CHAPTER)) %>%
    dplyr::select(
      parent,
      name = BNF_SECTION,
      value = ITEM_P_LEVEL2
    ) %>%
    dplyr::mutate(id = as.character(dplyr::row_number()))

  plot_df <- dplyr::bind_rows(df1, df2) %>%
    highcharter::list_parse()

  # Pyramid plot for age band and gender
  output$bnf_chapter_carehome_treemap <- highcharter::renderHighchart({
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
# mod_bnf_chapter_carehome_treemap_ui("bnf_chapter_carehome_treemap_1")

## To be copied in the server
# callModule(mod_bnf_chapter_carehome_treemap_server, "bnf_chapter_carehome_treemap_1")
