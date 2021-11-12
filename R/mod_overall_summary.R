#' overall_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_overall_summary_ui <- function(id) {
  ns <- NS(id)

  # Pull dataframes for valueBox
  items_per_patient_df <-
    careHomePrescribingScrollytellR::items_per_patient_df %>%
    dplyr::filter(is.na(YEAR_MONTH)) %>%
    dplyr::mutate(ITEMS_PER_PATIENT = round(ITEMS_PER_PATIENT, 0)) %>%
    tidyr::pivot_wider(
      names_from = CH_FLAG, values_from = ITEMS_PER_PATIENT
    ) %>%
    dplyr::mutate(ICON = "prescription")

  cost_per_patient_df <-
    careHomePrescribingScrollytellR::cost_per_patient_df %>%
    dplyr::filter(is.na(YEAR_MONTH)) %>%
    dplyr::mutate(COST_PER_PATIENT = round(COST_PER_PATIENT, 1)) %>%
    tidyr::pivot_wider(names_from = CH_FLAG, values_from = COST_PER_PATIENT) %>%
    dplyr::mutate(ICON = "coins")

  unique_medicines_per_patient_df <-
    careHomePrescribingScrollytellR::unique_medicines_per_patient_df %>%
    dplyr::filter(is.na(YEAR_MONTH)) %>%
    dplyr::mutate(
      UNIQUE_MEDICINES_PER_PATIENT = round(UNIQUE_MEDICINES_PER_PATIENT, 0)
    ) %>%
    tidyr::pivot_wider(
      names_from = CH_FLAG,
      values_from = UNIQUE_MEDICINES_PER_PATIENT
    ) %>%
    dplyr::mutate(ICON = "tablets")

  tagList(
    h3("Estimated prescribing patterns of care home residents and older population"),
    h6(em("All metrics are calculated per patient, per month")),
    br(),
    fluidRow(
      h6("Items"),
      mod_value_box_ui(
        id = "1",
        care_home = TRUE,
        value = items_per_patient_df$`Care home`,
        icon = items_per_patient_df$ICON
      ),
      mod_value_box_ui(
        id = "2",
        care_home = FALSE,
        value = items_per_patient_df$`Non care home`,
        icon = items_per_patient_df$ICON
      )
    ),
    fluidRow(
      h6("Drug Cost (£)"),
      mod_value_box_ui(
        id = "3",
        care_home = TRUE,
        value = cost_per_patient_df$`Care home`,
        icon = cost_per_patient_df$ICON
      ),
      mod_value_box_ui(
        id = "4",
        care_home = FALSE,
        value = cost_per_patient_df$`Non care home`,
        icon = cost_per_patient_df$ICON
      )
    ),
    fluidRow(
      h6("Unique Medicines"),
      mod_value_box_ui(
        id = "5",
        care_home = TRUE,
        value = unique_medicines_per_patient_df$`Care home`,
        icon = unique_medicines_per_patient_df$ICON
      ),
      mod_value_box_ui(
        id = "6",
        care_home = FALSE,
        value = unique_medicines_per_patient_df$`Non care home`,
        icon = unique_medicines_per_patient_df$ICON
      )
    )
  )
}

#' slider Server Function
#'
#' @noRd
mod_overall_summary_server <- function(input, output, session) {
  ns <- session$ns
}

## To be copied in the UI
# mod_overall_summary_ui("overall_summary_1")

## To be copied in the server
# callModule(mod_overall_summary_server, "overall_summary_1")
