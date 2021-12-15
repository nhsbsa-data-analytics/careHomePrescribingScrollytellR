#' download_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(
      outputId = ns("downloaddata"),
      label = "Download Data"
    )
  )
}

#' download_data Server Functions
#'
#' @noRd
mod_download_data_server <- function(id, export_data) { # didn't work...
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    output$downloaddata <- downloadHandler(
      filename = function() {
        paste(Sys.time(), "download.csv")
      },
      content = function(file) {
        write.csv(export_data(), file)
      }
    )
  })
}

## To be copied in the UI
# mod_download_data_ui("download_data_ui_1")

## To be copied in the server
# mod_download_data_server("download_data_ui_1")
