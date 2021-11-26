#' 02_overall_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_overall_summary_ui <- function(id) {
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
    dplyr::mutate(COST_PER_PATIENT = round(COST_PER_PATIENT, 0)) %>%
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
    h3("Estimated prescribing patterns of care home residents and older care home residents"),
    p(
      "Care home patients receive more prescription items than non-care home patients",
      " aged 65+ receiving prescriptions. We estimate",
      tags$b("10 prescription items"), " per patient per month at an estimated cost of £90 per patient per month.",
      "This compares to 6 items per patient per month at a cost of £47 per patient per month for non-care home patients aged 65+ receiving prescriptions."
    ),
    p("Correspondingly the estimated volumne and number of medicines per patient per month is higher."),
    # h6(em("All metrics are calculated per patient, per month")),
    # h6(em("Values are shown 65+ care home residents receiving prescription, and 65+ patients receiving prescriptions")),
    br(),
    fluidRow(
      column(
        width = 6, #
        h6("65+ care home residents receiving prescriptions", style = "margin-bottom: 10px")
      ),
      column(
        width = 6, #
        h6("65+ non-care home patients receiving prescriptions", style = "text-indent:3px;margin-bottom: 10px")
      )
    ),
    fluidRow(
      p("Average number of prescription items per patient per month", style = "text-indent: 20px;margin-bottom: 10px"),
      column(
        width = 6, offset = 0, style = "padding-left:0px", #
        # p("Average number of prescription items per patient per month in care home"),
        mod_value_box_ui(
          id = "1",
          care_home = TRUE,
          value = items_per_patient_df$`Care home`,
          icon = items_per_patient_df$ICON
        ),
      ),
      column(
        width = 6, #
        # p("Average number of prescription items per patient per month"),
        mod_value_box_ui(
          id = "2",
          care_home = FALSE,
          value = items_per_patient_df$`Non care home`,
          icon = items_per_patient_df$ICON
        )
      )
    ),
    fluidRow(
      p("Average drug cost per patient per month (£)", style = "text-indent: 20px;margin-bottom: 10px"),
      column(
        width = 6, offset = 0, style = "padding-left:0px", #
        # p("Average drug cost per patient in care home"),
        mod_value_box_ui(
          id = "3",
          care_home = TRUE,
          value = cost_per_patient_df$`Care home`,
          icon = cost_per_patient_df$ICON
        )
      ),
      column(
        width = 6,
        # p("Average drug cost per patient"),
        mod_value_box_ui(
          id = "4",
          care_home = FALSE,
          value = cost_per_patient_df$`Non care home`,
          icon = cost_per_patient_df$ICON
        )
      )
    ),
    fluidRow(
      p("Average number of unique medicines per patient per month", style = "text-indent: 20px;margin-bottom: 10px"),
      column(
        width = 6, offset = 0, style = "padding-left:0px", #
        # p("Average number of unique medicines per patient per month in care home"),
        mod_value_box_ui(
          id = "5",
          care_home = TRUE,
          value = unique_medicines_per_patient_df$`Care home`,
          icon = unique_medicines_per_patient_df$ICON
        )
      ),
      column(
        width = 6,
        # p("Average number of unique medicines per patient"),
        mod_value_box_ui(
          id = "6",
          care_home = FALSE,
          value = unique_medicines_per_patient_df$`Non care home`,
          icon = unique_medicines_per_patient_df$ICON
        )
      )
    ),
    fluidRow(
      p("Ten or more unique medicines", style = "text-indent: 20px;margin-bottom: 10px"),
      column(
        width = 6, offset = 0, style = "padding-left:0px", #
        # p("Average number of unique medicines per patient per month in care home"),
        mod_value_box_ui(
          id = "7",
          care_home = TRUE,
          value = ten_or_more_unique_medicines_per_patient_df$`Care home`,
          icon = ten_or_more_unique_medicines_per_patient_df$ICON
        )
      ),
      column(
        width = 6,
        # p("Average number of unique medicines per patient"),
        mod_value_box_ui(
          id = "8",
          care_home = FALSE,
          value = ten_or_more_unique_medicines_per_patient_df$`Non Care home`,
          icon = ten_or_more_unique_medicines_per_patient_df$ICON
        )
      )
    )
  )
}

#' slider Server Function
#'
#' @noRd
mod_02_overall_summary_server <- function(input, output, session) {
  ns <- session$ns
}

## To be copied in the UI
# mod_02_overall_summary_ui("02_overall_summary_1")

## To be copied in the server
# callModule(mod_02_overall_summary_server, "02_overall_summary_1")
