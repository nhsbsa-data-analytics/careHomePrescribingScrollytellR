#' 03_overall_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_03_overall_summary_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h4(
      "Estimated prescribing patterns for ",
      tippy(
        text = "older care home patients",
        tooltip = tooltip_text$care_home
      ),
      " compared to ",
      tippy(
        text = "older non-care home patients",
        tooltip = tooltip_text$non_care_home
      )
    ),
    p(
      "Overall, care home patients receive more prescription items and unique medicines per patient per month than non-care home patients",
      " aged 65+ receiving prescriptions. We estimate",
      tags$b("10 prescription items"), " per patient per month at an estimated cost of £90 per patient per month.",
      "This compares to 6 items per patient per month at a cost of £47 per patient per month for non-care home patients aged 65+ receiving prescriptions."
    ),
    p("Correspondingly the estimated volumne and number of medicines per patient per month is higher."),
    br(),
    fluidRow(
      style = "background-color: #FFFFFF;",
      col_6(
        selectInput(
          inputId = ns("geography"),
          label = "Geography",
          choices = c("Overall", "Region", "STP", "Local Authority"),
          width = "100%"
        )
      ),
      col_6(
        selectInput(
          inputId = ns("sub_geography"),
          label = "Sub Geography",
          choices = NULL, # dynamically generated
          width = "100%"
        )
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
      uiOutput(ns("table"))
    )
  )
}

#' 03_overall_summary Server Function
#'
#' @noRd
mod_03_overall_summary_server <- function(input, output, session) {
  ns <- session$ns
  
  # Join the 2 metric datasets together
  overall_summary_df <- 
    dplyr::full_join(
      x = careHomePrescribingScrollytellR::items_and_cost_per_patient_by_geography_and_ch_flag_df,
      y = careHomePrescribingScrollytellR::unique_medicines_per_patient_by_geography_df
    )
  
  # Only interested in overall period
  overall_summary_df <- overall_summary_df %>%
    dplyr::filter(YEAR_MONTH == "Overall")
  
  # Filter to relevant data for this chart
  overall_summary_df <- overall_summary_df %>%
    dplyr::filter(dplyr::across(c(GEOGRAPHY, SUB_GEOGRAPHY_NAME), not_na))
  
  # Tidy the cols
  overall_summary_df <- overall_summary_df %>%
    dplyr::mutate(
      COST_PER_PATIENT = paste0(
        "£", 
        format(x = round(COST_PER_PATIENT, 2), nsmall = 2)
      ),
      ITEMS_PER_PATIENT = round(ITEMS_PER_PATIENT, 0),
      UNIQUE_MEDICINES_PER_PATIENT = round(UNIQUE_MEDICINES_PER_PATIENT, 0),
      PCT_PATIENTS_TEN_OR_MORE = paste0(round(PCT_PATIENTS_TEN_OR_MORE, 0), "%")
    )
  
  # Handy resource: https://mastering-shiny.org/action-dynamic.html
  
  # Filter the data based on the geography
  geography_df <- reactive({
    req(input$geography)
    overall_summary_df %>%
      dplyr::filter(GEOGRAPHY == input$geography)
    
  })
  
  # Update the list of choices for sub geography from the rows in the geography 
  # dataframe
  observeEvent(
    eventExpr = geography_df(), 
    handlerExpr = {
      updateSelectInput(
        inputId = "sub_geography", 
        choices = unique(geography_df()$SUB_GEOGRAPHY_NAME)
      ) 
    }
  )
  
  # Filter the data based on the level and format for the table
  table_df <- reactive({
    req(input$sub_geography)
    geography_df() %>%
      dplyr::filter(SUB_GEOGRAPHY_NAME == input$sub_geography)
  })
  
  # Create the table
  output$table <- renderUI({
    req(input$sub_geography)
    tagList(
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
            value = table_df() %>% 
              dplyr::filter(CH_FLAG == "Care home") %>% 
              dplyr::pull(COST_PER_PATIENT),
            icon = "coins"
          )
        ),
        col_3(
          mod_value_box_ui(
            id = "4",
            care_home = FALSE,
            value = table_df() %>% 
              dplyr::filter(CH_FLAG == "Non care home") %>% 
              dplyr::pull(COST_PER_PATIENT),
            icon = "coins"
          )
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
            value = table_df() %>% 
              dplyr::filter(CH_FLAG == "Care home") %>% 
              dplyr::pull(ITEMS_PER_PATIENT),
            icon = "prescription"
          ),
        ),
        col_3(
          mod_value_box_ui(
            id = "2",
            care_home = FALSE,
            value = table_df() %>% 
              dplyr::filter(CH_FLAG == "Non care home") %>% 
              dplyr::pull(ITEMS_PER_PATIENT),
            icon = "prescription"
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
            value = table_df() %>% 
              dplyr::filter(CH_FLAG == "Care home") %>% 
              dplyr::pull(UNIQUE_MEDICINES_PER_PATIENT),
            icon = "pills"
          )
        ),
        col_3(
          mod_value_box_ui(
            id = "6",
            care_home = FALSE,
            value = table_df() %>% 
              dplyr::filter(CH_FLAG == "Non care home") %>% 
              dplyr::pull(UNIQUE_MEDICINES_PER_PATIENT),
            icon = "pills"
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
            value = table_df() %>% 
              dplyr::filter(CH_FLAG == "Care home") %>% 
              dplyr::pull(PCT_PATIENTS_TEN_OR_MORE),
            icon = "pills"
          )
        ),
        col_3(
          mod_value_box_ui(
            id = "8",
            care_home = FALSE,
            value = table_df() %>% 
              dplyr::filter(CH_FLAG == "Non care home") %>% 
              dplyr::pull(PCT_PATIENTS_TEN_OR_MORE),
            icon = "pills"
          )
        )
      )
    )
    
  })
  
}

## To be copied in the UI
# mod_03_overall_summary_ui("03_overall_summary_1")

## To be copied in the server
# callModule(mod_03_overall_summary_server, "03_overall_summary_1")
