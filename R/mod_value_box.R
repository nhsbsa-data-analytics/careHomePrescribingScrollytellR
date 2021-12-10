#' value_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_value_box_ui <- function(id,
                             value,
                             subtitle = "",
                             icon_name,
                             care_home) {
  ns <- NS(id)

  if (care_home) {
    color <- "#005EB8"
  } else {
    color <- "#768692"
  }

  tagList(
    div(
      class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
      div(
        class = "panel panel-white",
        div(
          class = "panel-heading",
          style = paste0("background-color:", color),
          div(
            class = "row",
            div(
              class = "col-xs-4 col-sm-4 col-md-4 col-lg-4",
              icon(icon_name, "fa-3x"),
              style = "color: #FFFFFF"
            ),
            div(
              class = "col-xs-8 col-sm-8 col-md-8 col-lg-8",
              div(
                style = "font-size: 28px; font-weight: bold; color: #FFFFFF; text-align: right;",
                value
              )
            )
          )
        )
      )
    )
  )
}

#' slider Server Function
#'
#' @noRd
mod_value_box_server <- function(input, output, session) {
  ns <- session$ns
}

## To be copied in the UI
# mod_value_box_ui("value_box_1")

## To be copied in the server
# callModule(mod_value_box_server, "value_box_1")
