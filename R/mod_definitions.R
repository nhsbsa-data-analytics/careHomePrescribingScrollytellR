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
    h6("Nursing home"),
    p(
      "A nursing home is a care home where a qualified nurse is provided to ",
      "ensure that the full needs of the person using the service are met."
    ),
    br(),
    h6("Residential home"),
    p(
      "A residential is a care home where a qualified nurse is not provided."
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
    h6("BNF code"),
    p(
      "A BNF code is a fifteen character hierarchical code used to identify ",
      "medicines based on the ",
      enurl(
        text = "British National Formulary (BNF)",
        url = "https://www.bnf.org/products/bnf-online/"
      ),
      " classifications."
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
