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
      "Overall, older care home patients receive around 1.4 times more prescription items  ",
      "and unique medicines per patient per month than non-care home older patients.",
      " At around twice the drug cost."
    ),
    br(),
    fluidRow(
      style = "background-color: #FFFFFF;",
      col_6(
        selectInput(
          inputId = ns("breakdown"),
          label = "Breakdown",
          choices = names(careHomePrescribingScrollytellR::breakdowns),
          width = "100%"
        )
      ),
      col_6(
        selectInput(
          inputId = ns("sub_breakdown"),
          label = "Sub Breakdown",
          choices = NULL, # dynamically generated
          width = "100%"
        )
      ),
      br(),
      fluidRow(
        col_6(
          style = "text-indent: 15px;",
          h6("Metric")
        ),
        col_3(
          style = "text-align: center;",
          h6("Care home")
        ),
        col_3(
          style = "text-align: center;",
          h6("Non-care home")
        )
      ),
      uiOutput(ns("table"))
    ),
    p(
      "Drug costs and volumes are higher for care home patients than non-care home patients ",
      "across all age and gender groups except females aged 90+ years. The greatest differences ",
      "are between younger care home and non-care home patients."
    )
  )
}

#' 03_overall_summary Server Functions
#'
#' @noRd
mod_03_overall_summary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Join the 2 metric datasets together
    metric_df <-
      dplyr::full_join(
        x = careHomePrescribingScrollytellR::items_and_cost_per_patient_by_breakdown_and_ch_flag_df,
        y = careHomePrescribingScrollytellR::unique_medicines_per_patient_by_breakdown_and_ch_flag_df
      )

    # Only interested in the overall period
    metric_df <- metric_df %>%
      dplyr::filter(YEAR_MONTH == "Overall")
    
    # Format cost and percentage cols
    metric_df <- metric_df %>%
      dplyr::mutate(
        SDC_COST_PER_PATIENT = paste0("£", SDC_COST_PER_PATIENT),
        SDC_PCT_PATIENTS_TEN_OR_MORE = paste0(SDC_PCT_PATIENTS_TEN_OR_MORE, "%")
      )

    # Handy resource: https://mastering-shiny.org/action-dynamic.html

    # Filter the data based on the breakdown
    breakdown_df <- reactive({
      
      req(input$breakdown)

      metric_df %>%
        dplyr::filter(BREAKDOWN == input$breakdown)
      
    })
    
    # Pull the text for the number of NA sub breakdown patients
    patients_in_na_sub_breakdown_text <- reactive({
      
      req(input$breakdown)
      
      # Filter to NA sub breakdown
      na_sub_breakdown_df <- breakdown_df() %>%
        dplyr::filter(is.na(SUB_BREAKDOWN_NAME))
      
      # Apply SDC to total patients
      na_sub_breakdown_df <- na_sub_breakdown_df %>%
        dplyr::mutate(
          SDC = ifelse(TOTAL_PATIENTS %in% c(1, 2, 3, 4), 1, 0),
          SDC_TOTAL_PATIENTS = 
            ifelse(SDC == 1, NA_integer_, round(TOTAL_PATIENTS, -1))
        )
      
      # Format the number
      na_sub_breakdown_df <- na_sub_breakdown_df %>%
        dplyr::mutate(
          SDC_TOTAL_PATIENTS = ifelse(
            test = is.na(SDC_TOTAL_PATIENTS), 
            yes = "c", 
            no = as.character(SDC_TOTAL_PATIENTS)
          )
        ) 
      
      # Extract the values
      patients_in_na_sub_breakdown <- na_sub_breakdown_df %>%
        dplyr::pull(SDC_TOTAL_PATIENTS, CH_FLAG)
      
      if (length(patients_in_na_sub_breakdown) == 2) {
        # If both care home and non care home exist
        
        paste(
          "This excludes", patients_in_na_sub_breakdown[1], "care home and", 
          patients_in_na_sub_breakdown[2], "non care home patients with an ",
          "unknown sub breakdown."
        )
        
      } else if (length(patients_in_na_sub_breakdown) == 1) {
        # If only one exists
        paste(
          "This excludes", patients_in_na_sub_breakdown, 
          tolower(names(patients_in_na_sub_breakdown)), "patients with an ",
          "unknown sub breakdown."
        )
        
      } else {
        # Nothing
        ""
        
      }

    })

    # Update the list of choices for sub breakdown from the rows in breakdown
    # dataframe
    observeEvent(
      eventExpr = breakdown_df(),
      handlerExpr = {
        freezeReactiveValue(input, "sub_breakdown")
        updateSelectInput(
          inputId = "sub_breakdown",
          choices = breakdown_df()$SUB_BREAKDOWN_NAME %>%
            na.omit() %>%
            unique()
        )
      }
    )

    # Filter the data based on the sub breakdown
    table_df <- reactive({
      
      req(input$sub_breakdown)

      breakdown_df() %>%
        dplyr::filter(SUB_BREAKDOWN_NAME == input$sub_breakdown)
      
    })

    # Create the table
    output$table <- renderUI({
      
      req(input$sub_breakdown)
      
      tagList(
        fluidRow(
          col_6(
            style = "text-indent: 15px;",
            p(
              tippy(
                text = "Drug cost",
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
                dplyr::pull(SDC_COST_PER_PATIENT),
              icon = "coins"
            )
          ),
          col_3(
            mod_value_box_ui(
              id = "4",
              care_home = FALSE,
              value = table_df() %>%
                dplyr::filter(CH_FLAG == "Non care home") %>%
                dplyr::pull(SDC_COST_PER_PATIENT),
              icon = "coins"
            )
          )
        ),
        fluidRow(
          col_6(
            style = "text-indent: 15px;",
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
                dplyr::pull(SDC_ITEMS_PER_PATIENT),
              icon = "prescription"
            ),
          ),
          col_3(
            mod_value_box_ui(
              id = "2",
              care_home = FALSE,
              value = table_df() %>%
                dplyr::filter(CH_FLAG == "Non care home") %>%
                dplyr::pull(SDC_ITEMS_PER_PATIENT),
              icon = "prescription"
            )
          )
        ),
        fluidRow(
          col_6(
            style = "text-indent: 15px;",
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
                dplyr::pull(SDC_UNIQUE_MEDICINES_PER_PATIENT),
              icon = "pills"
            )
          ),
          col_3(
            mod_value_box_ui(
              id = "6",
              care_home = FALSE,
              value = table_df() %>%
                dplyr::filter(CH_FLAG == "Non care home") %>%
                dplyr::pull(SDC_UNIQUE_MEDICINES_PER_PATIENT),
              icon = "pills"
            )
          )
        ),
        fluidRow(
          col_6(
            style = "text-indent: 15px;",
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
                dplyr::pull(SDC_PCT_PATIENTS_TEN_OR_MORE),
              icon = "pills"
            )
          ),
          col_3(
            mod_value_box_ui(
              id = "8",
              care_home = FALSE,
              value = table_df() %>%
                dplyr::filter(CH_FLAG == "Non care home") %>%
                dplyr::pull(SDC_PCT_PATIENTS_TEN_OR_MORE),
              icon = "pills"
            )
          )
        ),
        fluidRow(
          col_12(
            p(
              style = "font-size: 12px; text-align: right; padding-right: 15px;",
              patients_in_na_sub_breakdown_text()
            )
          )
        )
      )
    })
  })
}

## To be copied in the UI
# mod_03_overall_summary_ui("03_overall_summary_ui_1")

## To be copied in the server
# mod_03_overall_summary_server("03_overall_summary_ui_1")
