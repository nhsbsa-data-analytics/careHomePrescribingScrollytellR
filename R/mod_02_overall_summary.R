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
    dplyr::mutate(
      COST_PER_PATIENT = paste0("£", round(COST_PER_PATIENT, 0))
    ) %>%
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
  
  ten_or_more_unique_medicines_per_patient_df <-
    careHomePrescribingScrollytellR::ten_or_more_unique_medicines_per_patient_df %>%
    dplyr::filter(is.na(YEAR_MONTH)) %>%
    dplyr::mutate(
      PCT_PATIENTS_TEN_OR_MORE = paste0(round(PCT_PATIENTS_TEN_OR_MORE, 0), "%")
    ) %>%
    tidyr::pivot_wider(
      names_from = CH_FLAG,
      values_from = PCT_PATIENTS_TEN_OR_MORE
    ) %>%
    dplyr::mutate(ICON = "tablets")

  tagList(
    h4(
      "Estimated prescribing patterns of ",
      tippy(
        text = "older care home patients", 
        tooltip = tooltip_text$care_home
      ),
      " compared to ",
      tippy(
        text = "older non-care home patients", 
        tooltip = tooltip_text$non_care_home
      ),
      "."
    ),
    br(),
    fluidRow(
      col_3(
        offset = 6,
        h6("Care home")
      ),
      col_3(
        h6("Non-care home")
      )
    ),
    fluidRow(
      col_6(
        p(
          tippy(
            text = "Number of prescription items", 
            tooltip = tooltip_text$items
          )
        )
      ),
      col_3(
        mod_value_box_ui(
          id = "1",
          care_home = TRUE,
          value = items_per_patient_df$`Care home`,
          icon = items_per_patient_df$ICON
        ),
      ),
      col_3(
        mod_value_box_ui(
          id = "2",
          care_home = FALSE,
          value = items_per_patient_df$`Non care home`,
          icon = items_per_patient_df$ICON
        )
      )
    ),
    fluidRow(
      col_6(
        p(
          tippy(
            text = "Total drug cost", 
            tooltip = tooltip_text$cost
          )
        )
      ),
      col_3(
        mod_value_box_ui(
          id = "3",
          care_home = TRUE,
          value = cost_per_patient_df$`Care home`,
          icon = cost_per_patient_df$ICON
        )
      ),
      col_3(
        mod_value_box_ui(
          id = "4",
          care_home = FALSE,
          value = cost_per_patient_df$`Non care home`,
          icon = cost_per_patient_df$ICON
        )
      )
    ),
    fluidRow(
      col_6(
        p(
          tippy(
            text = "Number of unique medicines", 
            tooltip = tooltip_text$unique_medicines
          )
        )
      ),
      col_3(
        mod_value_box_ui(
          id = "5",
          care_home = TRUE,
          value = unique_medicines_per_patient_df$`Care home`,
          icon = unique_medicines_per_patient_df$ICON
        )
      ),
      col_3(
        mod_value_box_ui(
          id = "6",
          care_home = FALSE,
          value = unique_medicines_per_patient_df$`Non care home`,
          icon = unique_medicines_per_patient_df$ICON
        )
      )
    ),
    fluidRow(
      col_6(
        p(
          tippy(
            text = "Patients on ten or more unique medicines", 
            tooltip = tooltip_text$ten_or_more_unique_medicines
          )
        )
      ),
      col_3(
        mod_value_box_ui(
          id = "7",
          care_home = TRUE,
          value = ten_or_more_unique_medicines_per_patient_df$`Care home`,
          icon = ten_or_more_unique_medicines_per_patient_df$ICON
        )
      ),
      col_3(
        mod_value_box_ui(
          id = "8",
          care_home = FALSE,
          value = ten_or_more_unique_medicines_per_patient_df$`Non care home`,
          icon = ten_or_more_unique_medicines_per_patient_df$ICON
        )
      )
    )
  )
}

#' 02_overall_summary Server Function
#'
#' @noRd
mod_02_overall_summary_server <- function(input, output, session) {
  ns <- session$ns
}

## To be copied in the UI
# mod_02_overall_summary_ui("02_overall_summary_1")

## To be copied in the server
# callModule(mod_02_overall_summary_server, "02_overall_summary_1")
