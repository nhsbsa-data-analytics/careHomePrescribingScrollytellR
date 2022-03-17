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
    h1("Definitions"),
    p(tags$b("Older patients")),
    p("Patients aged 65+ at the time of prescribing."),
    p(tags$b("Older care home patients")),
    p(
      "Patients aged 65+ who received their prescription whilst living in a ",
      "care home at the time of prescribing."
    ),
    p(tags$b("Older non-care home patients")),
    p(
      "Patients aged 65+ who received their prescription whilst not living in ",
      "a care home at the time of prescribing."
    ),
    p(tags$b("Nursing home")),
    p(
      "A nursing home is a care home where a qualified nurse is provided to ",
      "ensure that the full needs of the person using the service are met."
    ),
    p(tags$b("Residential home")),
    p("A residential is a care home where a qualified nurse is not provided."),
    p(tags$b("Per Patient Month")),
    p(
      "The Per Patient Month metrics were calculated by summing the total cost ,",
      "number of itmes, and number of unique medicines for each patient across ",
      "the period. This total was then divided by the number of months in the ",
      "period where the patient was attributed prescribing. The mean of these ",
      "values is then taken across all patients to give each per patient month ",
      "metric."
      ),
    p(tags$b("Drug cost")),
    p(
      "Calculated as the average amount paid for dispensed drugs ",
      "per patient month. It relates solely to the basic price of the drugs, ",
      "in the quantity prescribed on a prescription form. ",
      "It does not include any additional fees or discounts that were paid to ",
      "the dispensing contractors. Also known as Net Ingredient Cost (NIC)."
    ),
    p(tags$b("Number of prescription items")),
    p(
      "Calculated as the average number of prescription items per patient ",
      "month. A count of the number of times a product, such as a drug or ",
      "appliance, appears on a prescription form. It does not account for ",
      "dosage or quantity prescribed. For example, a patient could receive ",
      "100 x 50mg tablets as an item and another could receive 7 x 5 mg ",
      "tablets as an item. Both would be counted as 1 item"
    ),
    p(tags$b("Number of unique medicines")),
    p(
      "Calculated as the average number of unique medicines per patient ",
      "month. A unique medicine is defined as a medicine prescribed with the ",
      "same chemical substance descriptor in BNF Chapters 1 to 4 and 6 to 10 ",
      "whether it be different formulations (presentations) or different ",
      "strengths. Medicines with the same chemical substance descriptor would ",
      "be counted as one (single) unique product e.g. Warfarin 1mg, 3mg and ",
      "5mg tablets."
    ),
    p(tags$b("Patients on ten or more unique medicines")),
    p(
      "Calculated as the average percentage of patients prescribed ten or ",
      "more unique medicines per month. A unique medicine is defined as a ",
      "medicine prescribed with the same chemical substance descriptor in BNF ",
      "Chapters 1 to 4 and 6 to 10 whether it be different formulations ",
      "(presentations) or different strengths. Medicines with the same ",
      "chemical substance descriptor would be counted as one (single) unique ",
      "product e.g. Warfarin 1mg, 3mg and 5mg tablets."
    ),
    p(tags$b("BNF code")),
    p(
      "A BNF code is a fifteen character hierarchical code used to identify ",
      "medicines based on the ",
      enurl(
        text = "British National Formulary (BNF)",
        url = "https://www.bnf.org/products/bnf-online/"
      ),
      " classifications."
    ),
    p(
      "The ",
      enurl(
        text = "British National Formulary (BNF)",
        url = "https://www.bnf.org/products/bnf-online/"
      ),
      " is a reference book containing the standard list of medicines used in ",
      "UK prescribing. It gives information on the indications, dosages and ",
      "side effects for over 70,000 medicines. The BNF used to show medicines ",
      "in a hierarchy, and the ",
      enurl(
        text = "NHS Business Services Authority",
        url = "https://www.nhsbsa.nhs.uk/"
      ),
      " use a legacy version of the BNF hierarchy to assign codes to drugs ",
      "and chemicals."
    ),
    p(
      "The code breakdown as follows: ",
      tags$ul(
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
