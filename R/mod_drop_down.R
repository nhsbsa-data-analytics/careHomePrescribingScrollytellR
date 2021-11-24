#' drop_down UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_drop_down_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput("geo_level1"),
    uiOutput("geo_level2")
  )
}

#' drop_down Server Functions
#'
#' @noRd
mod_drop_down_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$geo_level1 <- renderUI({
      selectInput(
        "selected_geography",
        label = h4("Choose geography"),
        choices = list(
          "STP" = "STP",
          "Local Authority" = "LA"
        ),
        selected = "STP"
      )
    })

    getList <- reactive({
      if (input$selected_geography == "STP") {
        return(stp_list)
      } else if (input$selected_geography == "LA") {
        return(la_list)
      }
    })

    output$geo_level2 <- renderUI({
      pickerInput(
        "selected_sub_geography",
        label = h4("Select sub geography"),
        choices = as.list(getList()),
        selected = "Overall"
      )
    })
  })
}

## To be copied in the UI
# mod_drop_down_ui("drop_down_ui_1")

## To be copied in the server
# mod_drop_down_server("drop_down_ui_1")
