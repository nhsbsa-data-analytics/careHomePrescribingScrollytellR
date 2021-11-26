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
      class = "col-lg-9 col-md-9",
      div(
        class = "panel panel-white",
        div(
          class = "panel-heading",
          style = paste0("background-color:", color),
          div(
            class = "row",
            div(
              class = "col-md-6",
              icon(icon_name, "fa-3x"),
              style = "color: #FFFFFF"
            ),
            div(
              class = ("col-xs-9 text-right"),
              div(
                style = ("font-size: 40px;
                         font-weight: bold;
                         color: #FFFFFF"), value
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
