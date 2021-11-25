#' slider UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_slider_ui <- function(id,
                          label = "Year Month:",
                          min = "Overall",
                          max = "202103",
                          selected = "Overall",
                          width = "100%") {
  ns <- NS(id)

  yearmonth <- c(
    "Overall",
    "202004",
    "202005",
    "202006",
    "202007",
    "202008",
    "202009",
    "202010",
    "202011",
    "202012",
    "202101",
    "202102",
    "202103"
  )

  stopifnot(min %in% yearmonth)
  stopifnot(max %in% yearmonth)
  stopifnot(selected %in% yearmonth)

  tagList(
    shinyWidgets::sliderTextInput(
      inputId = ns("slider"),
      label = label,
      choices = yearmonth[which(yearmonth == min):which(yearmonth == max)],
      selected = selected,
      animate = TRUE,
      width = width
    )
  )
}

#' slider Server Functions
#'
#' @noRd
mod_slider_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    return(input$slider)
  })
}

## To be copied in the UI
# mod_slider_ui("slider_ui_1")

## To be copied in the server
# mod_slider_server("slider_ui_1")
