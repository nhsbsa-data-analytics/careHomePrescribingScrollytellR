#' 01_intro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_01_intro_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Estimated prescribing patterns for care home patients aged 65 years or over"),
    p(
      "In this article we provide unique insight into primary care ",
      "prescribing patterns for care home patients aged 65 years or over",
      " in England during 2020/21."
    ),
    p(
      "This is based on experimental data linkage work. We welcome feedback, ",
      "collaboration and refinement of the methodology to see if it can be ",
      "used in NHSBSA information systems in the future and how we can develop ",
      "additional analyses."
    ),
    h2("Key Findings"),
    p(
      "We estimate that there was an average of",
      tags$b("289 thousand care home patients aged 65 years or over"),
      "receiving prescriptions each month in 2020/21. ",
      "They received an estimated", tags$b("35 million"),
      "prescription items", tags$b("at a cost of £324 million"), "during ",
      "2020/21."
    ),
    # Above possibly not as clear as it could be. First sentence is about per
    # month, but second sentence is about total for year. Could add estimated
    # prescriptions per month?
    p(
      "This accounts for around", tags$b("5%"), "of patients aged 65 or over ",
      "receiving prescription items each month and", tags$b("7%"), "of the ",
      "total primary care drug spend for patients aged 65 years or over during 2020/21."
    ),
    p(
      "As might be expected,",
      tags$b("care home patients receive more prescribing"), "than non-care ",
      "home patients receiving prescriptions. They also receive a different ",
      "range of medicines and are more likely to receive ",
      "prescribing for pain relief."
    ),
    p(
      "Average monthly prescribing costs and volumes per care home patient ",
      "vary by age, gender, care home type and geography."
    ),
    # When reading the Key Findings, the figures suggested to me that spend on
    # drugs for CH px's was ~7:5 (since 7% of total spend is on 5% of the
    # population of 65+ px's) = 1.4x compared to non-CH px's. To check my
    # understanding, from the report I see:
    #
    # "KEY FINDINGS We estimate that there was an average of 289 thousand care
    # home patients aged 65 years or over receiving prescriptions each month in
    # 2020/21. They received an estimated 35 million prescription items at a
    # cost of £324 million during 2020/21.
    #
    # This accounts for around 5% of patients aged 65 or over receiving
    # prescription items each month and 7% of the total primary care drug spend
    # for patients aged 65 years or over during 2020/21."
    #
    # => spend per CH px = (324M / 289k)/12 =~ £93, agreeing with figure in
    # first prescribing metrics value breakdown
    #
    # => 5491k non-CH px 65+ receiving. rx per month &  £4304M spend per year on
    # non-CH px rx's => £359M spend per month on non-CH px rx's
    #
    # => £65 spend per month per non-CH px, does not agree with figure in first
    # prescribing metrics value breakdown
    #
    # "The estimated average monthly drug cost for care home patients aged 65
    # years or over is around twice that for non-care home patients aged 65
    # years or over who received prescriptions." (L27-29 in mod_03_care_home_prescribing)
    #
    # -- So I do not see how the spend per px is ~2x? I must be missing something!
    h2(enurl(
      text = "Methodology",
      url = "https://rpubs.com/nhsbsa-data-analytics/methodology"
    )),
    p(
      "Prescribing estimates are based on a sophisticated methodology which ",
      "includes linking primary care prescription address data to care home ",
      "addresses ",
      "in ",
      enurl(
        text = "AddressBase Plus",
        url = "https://www.ordnancesurvey.co.uk/business-government/products/addressbase"
      ),
      "and ",
      enurl(
        text = "CQC data",
        url = "https://anypoint.mulesoft.com/exchange/portals/care-quality-commission-5/4d36bd23-127d-4acf-8903-ba292ea615d4/cqc-syndication-1/"
      ),
      "to create a prescribing dataset. The address matching method has been ",
      "made available as an ",
      enurl(
        text = "R package",
        url = "https://github.com/nhsbsa-data-analytics/addressMatchR"
      ),
      " and the methodology is published online. Throughout the project the NHSBSA ",
      "ensured data was protected and secure and adhered to the Cardicott guardian principles."
    ),
    p(
      "These estimates address a key gap in knowledge and give valuable ",
      "insights which could inform the use and management of medicines in ",
      "care homes to help improve health outcomes, the quality of care and ",
      "ensure value.",
      tags$b(
        "It should however be noted that the analysis period is at the height ",
        "of the COVID-19 pandemic and patterns may change in subsequent years."
      )
    ),
    p(
      "It is also important to note that these are estimates of ",
      "care home residents aged 65 years or over ",
      tags$b("receiving prescriptions, "),
      "and are referred to as care home patients. As such the estimates are ",
      "lower than for care home residents aged 65 years or over in general."
    )
    # I did not find this clear, until Adnan had explained it.
    # My notes when reading were:
    # I don't fully understand what this is saying, as the estimates are for
    # those receiving prescriptions how can they be lower than for the group of
    # care home residents NOT receiving prescriptions?* are lower than for care
    # home residents aged 65 years or over in general.
  )
}

#' 01_intro Server Functions
#'
#' @noRd
mod_01_intro_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_01_intro_ui("01_intro_ui_1")

## To be copied in the server
# mod_01_intro_server("01_intro_ui_1")
