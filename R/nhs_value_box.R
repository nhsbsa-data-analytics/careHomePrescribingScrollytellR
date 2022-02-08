#' nhs_value_box Function
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny tagList
nhs_value_box <- function(value, icon_name, group) {

  color <- switch(group,
    "care_home" = "#005EB8",
    "non_care_home" = "#768692",
    "nursing_home" = "#004281",
    "residential_home" = "#4D8ECD"
  )

  tagList(
    div(
      class = "nhsuk-card",
      style = paste0("margin-bottom: 10px; background:", color),
      div(
        class = "nhsuk-grid-row",
        style = "padding: 12px",
        div(
          class = "nhsuk-grid-column-one-third",
          style = "color: #FFFFFF;",
          icon(icon_name, "fa-2x"),
        ),
        div(
          class = "nhsuk-grid-column-two-thirds nhsuk-heading-m",
          style = "color: #FFFFFF; margin-bottom: 0px",
          value
        )
      )
    )
  )
}
