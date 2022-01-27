#' definitions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_definitions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Definitions"),
    br(),
    br(),
    h6("Older patients"),
    p(
      "Patients aged 65+ at the time of prescribing."
    ),
    br(),
    h6("Older care home patients"),
    p(
      "Patients aged 65+ who received their prescription whilst ",
      "living in a care home at the time of prescribing."
    ),
    br(),
    h6("Older non-care home patients"),
    p(
      "Patients aged 65+ who received their prescription ",
      "whilst not living in a care home at the time of prescribing."
    ),
    br(),
    h6("Drug cost"),
    p(
      "Calculated as the average total price reimbursed for dispensed drugs ",
      "per patient per month. It relates solely to the cost of the drugs, ",
      "in the quantity prescribed on a prescription form. ",
      "It does not include any additional fees or discounts that were ",
      "paid to the dispensing contractors."
    ),
    br(),
    h6("Number of prescription items"),
    p(
      "Calculated as the average number of prescription items ",
      "per patient per month. A count of the number of times a product, ",
      "such as a drug or appliance, appears on a prescription form. ",
      "It does not account for dosage or quantity prescribed. ",
      "For example, a patient could receive 100 x 50mg tablets as an ",
      "item and another could receive 7 x 5 mg tablets as an item. ",
      "Both would be counted as 1 item"
    ),
    br(),
    h6("Number of unique medicines"),
    p(
      "Calculated as the average number of unique medicines ",
      "per patient per month. A unique medicine is defined as ",
      "a medicine prescribed with the same chemical substance ",
      "descriptor in BNF* Sections 1 to 4 and 6 to 10 whether it be ",
      "different formulations (presentations) or different strengths. Medicines with the same chemical substance descriptor would be counted as one (single) unique product e.g. Warfarin 1mg, 3mg and 5mg tablets.",
    ),
    br(),
    h6("Patients on ten or more unique medicines"),
    p(
      "Calculated as the average percentage of patients prescribed ten ",
      "or more unique medicines per month. A unique medicine is defined ",
      "as a medicine prescribed with the same chemical substance descriptor ",
      "in BNF Sections 1 to 4 and 6 to 10 whether it be different ",
      "formulations (presentations) or different strengths. ",
      "Medicines with the same chemical substance descriptor ",
      "would be counted as one (single) unique product ",
      "e.g. Warfarin 1mg, 3mg and 5mg tablets."
    ),
    br(),
    h6("BNF"),
    p(
      "This is a fifteen character code, based on the ",
      enurl(
        text = "British National Formulary (BNF)",
        url = "https://www.bnf.org/products/bnf-online/"
      ),
      "classifications. The code breakdown as follows: ",
      tags$li("Char 1 - 2 - BNF Chapter"),
      tags$li("Char 3 - 4 - BNF Section"),
      tags$li("Char 5 - 6 - BNF Paragraph"),
      tags$li("Char 7 - BNF Sub-paragraph"),
      tags$li("Char 8 - 9 - Chemical substance"),
      tags$li("Char 10 - 11 - Drug or Product"),
      tags$li("Char 12 - 13 - Strength/Formulation"),
      tags$li("Char 14 - 15 - Equivalent drug")
    )
  )
}

#' definitions Server Functions
#'
#' @noRd
mod_definitions_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_definitions_ui("definitions_ui_1")

## To be copied in the server
# mod_definitions_server("definitions_ui_1")
