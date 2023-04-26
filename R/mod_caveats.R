#' caveats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_caveats_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Caveats"),
    p(
      "The caveats are grouped into five categories. Each group of caveats is ",
      "described within corresponding tab below."
    ),
    tabsetPanel(
      type = "tabs",
      tabPanel(
        title = "Prescriptions data",
        htmlOutput(outputId = ns("prescription"))
      ),
      tabPanel(
        title = "Patient data",
        htmlOutput(outputId = ns("patient"))
      ),
      tabPanel(
        title = "Lookup address",
        htmlOutput(outputId = ns("address"))
      ),
      tabPanel(
        title = "Address matching",
        htmlOutput(outputId = ns("matching"))
      ),
      tabPanel(
        title = "Metrics & Charts",
        htmlOutput(outputId = ns("metric"))
      )
    )
  )
}

#' caveats Server Functions
#'
#' @noRd
mod_caveats_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$prescription <- renderUI({
      tags$ul(
        br(),
        tags$li(
          "Analysis is based on primary care prescription data collected by the ",
          "NHS Business Services authority. The data are collected for the ",
          "operational purpose of reimbursing and remunerating dispensing ",
          "contractors for the costs of supplying drugs and devices, ",
          "along with essential and advanced services, to NHS patients. ",
          "This excludes:",
        ),
        tags$li(
          style = "margin-left: 35px; list-style-type: circle ;",
          "prescriptions that were issued but not presented for dispensing."
        ),
        tags$li(
          style = "margin-left: 35px; list-style-type: circle ;",
          "prescriptions that were not submitted to the NHSBSA for processing ",
          "and reimbursement."
        ),
        tags$li(
          style = "margin-left: 35px; list-style-type: circle ;",
          "prescriptions issued and dispensed in prisons, ",
          "hospitals and private prescriptions."
        ),
        tags$li(
          style = "margin-left: 35px; list-style-type: circle ;",
          "prescription batches submitted late."
        ),
        tags$li(
          "Prescription data relates to prescription batches submitted to the ",
          "NHSBSA for payment between April 2020 and March 2021. ",
          "The part month in NHSBSA data relates to the dispensing month ",
          "for which the prescription batch was submitted. This is generally ",
          "but not always the month in which the prescription was dispensed. ",
          "This means that there may be dispensing for given patients that has ",
          "not been submitted to the NHSBSA for payment and is therefore not ",
          "included. There may also be prescriptions included for a patient ",
          "that were dispensed prior to the dispensing month."
        ),
        tags$li(
          "Patients may receive prescription items that have not been ",
          "prescribed to them personally and will not be accounted for. ",
          "This may occur in the case of high-volume vaccines such as ",
          "flu vaccines, and in the case of bulk prescribing of products, ",
          "which can be bought in a pharmacy or supermarket such as ",
          "Lactulose syrup and small volumes of Paracetamol. ",
          "There is no means of quantifying the extent of this in NHSBSA data."
        ),
        tags$li(
          "The NHSBSA do not capture the clinical indication of a ",
          "prescription and therefore do not know the reason why a ",
          "prescription was issued, or the condition it is intended to treat. ",
          "Many drugs have multiple uses, and although classified in the BNF ",
          "by their primary therapeutic use may be issued to treat a ",
          "condition outside of this."
        ),
        tags$li(
          "Due to manual processes involved in the processing of ",
          "prescriptions there may be inaccuracies in capturing prescription ",
          "information which are then reflected in the data. ",
          "NHS Prescription Services have a variety of validation ",
          "streams throughout prescription processing to support ",
          "accurate capture of the data. In addition, a retrospective ",
          "sample is completed in the month following reimbursement to ",
          "identify the accuracy of prescription processing information. ",
          "The check includes the accuracy of prescriber, practice and drug ",
          "information, but does not include the personal details of ",
          "the patient. The latest reported Prescription Processing ",
          "Information Accuracy is 99.9%, which covers the 12-month ",
          "rolling period ending August 2021. The sample may not be ",
          "representative at a more granular level; as such the level of ",
          "accuracy is undetermined for specific groups such as drugs, ",
          "geographies, time periods etc. It should also be noted that ",
          "the identification of errors in the accuracy checking sample ",
          "does not result in amendments to data held in NHSBSA systems. ",
          "Further information on Prescription Processing Information Accuracy",
          "can be found ",
          enurl(
            text = "here.",
            url = "https://www.nhsbsa.nhs.uk/pharmacies-gp-practices-and-appliance-contractors/payments-and-pricing/how-we-process-prescriptions"
          )
        )
      )
    })

    output$patient <- renderUI({
      tags$ul(
        br(),
        tags$li(
          "The analysis focuses on prescriptions for older patients ",
          "aged 65 years or above at the time of prescribing. ",
          "Patient age was determined using a mixture of patient ",
          "information from prescription forms and ",
          enurl(
            text = "Personal Demographics Service (PDS)",
            url = "https://digital.nhs.uk/services/demographics "
          ),
          ". Further details on ",
          "the process of patient age determination can be found ",
          enurl(
            text = "here.",
            url = "https://www.nhsbsa.nhs.uk/sites/default/files/2018-02/180115%20Age%20Logic%20Summary%20Flow%20Chart%20-%20Revised%20Layout.pdf"
          )
        ),
        tags$li(
          "Patient prescription forms were labelled as being from a ",
          "care home or not based on address matching described in the ",
          "Methodology (accessed via the sidebar). Of 258 million prescription ",
          "forms issued to patients aged 65 and above, 16 million could be ",
          "categorised as being from a care home."
        ),
        tags$li(
          enurl(
            text = "Ordnance Survey AddressBase (AB) ",
            url = "https://www.ordnancesurvey.co.uk/business-government/products/addressbase"
          ),
          "is a product that is in a continual state of refinement. ",
          "While the epoch of AB used within the analysis was the closest ",
          "to the end of the analysis time period, there would still be ",
          "instances where the information in AB did not mirror actual patient ",
          "address details at the time of prescribing. "
        ),
        tags$li(
          "A selection of AB building classification types were removed from ",
          "the lookup data, such as street records, objects of interest, ",
          "car parks, garages and others. Some incorrect matches may have ",
          "occurred through not excluding other building classification types."
        ),
        tags$li(
          "The analysis only includes patients with an NHS number ",
          "and date of birth verified by PDS."
        ),
        tags$li(
          "NHS numbers are captured for 100% electronic prescription ",
          "messages. The estimated NHS number capture rate for paper ",
          "prescription forms is 83.7%."
        ),
        # Could add what the overall % of prescriptions included in the analysis
        # were.
        # OR
        # Add a final bullet point giving numbers and % of all possible
        # prescriptions included in the analysis, given the preceding caveats.
        tags$li(
          "The NHSBSA periodically investigate the accuracy of ",
          "NHS numbers captured from paper forms. The personal details ",
          "captured (NHS number, date of birth and age) are compared ",
          "against those on the prescription form for a random sample of ",
          "50,000 prescription forms. The NHS number captured typically ",
          "matches that on the prescription form for over 99.9% of forms. ",
          "The results represent the accuracy for all items processed; ",
          "as such the level of accuracy is undetermined for specific ",
          "medicines, geographies, time periods and other factors. ",
          "By contrast, the accuracy of captured NHS numbers in ",
          "electronic prescribing is estimated to be 100%."
        ),
        tags$li(
          "The analysis required that every prescription ",
          "form had a patient address recorded. Addresses were available ",
          "for the 85% of prescriptions which were electronic. ",
          "Address information was not captured directly from ",
          "paper prescriptions and therefore a process was ",
          "derived to generate these addresses using a mix of information ",
          "from the Personal Demographic Service (PDS) and electronic ",
          "prescriptions across a range of months This is described ",
          "in Section 2.3 of the Methodology. Although accurate, ",
          "this is not as robust as directly sourced patient address ",
          "information from electronic prescriptions."
        ),
        tags$li(
          "For the 2020/21 financial year, patient addresses could be ",
          "allocated for 99.7% of paper prescription forms where ",
          "the patient’s NHS number could be identified, and the patient ",
          "was aged 65+ years."
        ),
        tags$li(
          "Prescription forms with known non-English patient address ",
          "information were removed from the analysis. Records with an ",
          "unknown or missing postcode were included."
        )
      )
    })

    output$address <- renderUI({
      tags$ul(
        br(),
        tags$li(
          enurl(
            text = "Ordnance Survey AddressBase (AB) ",
            url = "https://www.ordnancesurvey.co.uk/business-government/products/addressbase"
          ),
          "was the foundation of the ",
          "lookup address data, which was matched against patient ",
          "address information."
        ),
        tags$li(
          "AB is a product that is in a continual state of refinement, ",
          "with updated releases on a six-weekly schedule. ",
          "The 15th March 2021 extract was used, as it was the closest ",
          "to the end of the analysis time period."
        ),
        tags$li(
          enurl(
            text = "AB building classifications ",
            url = "https://www.ordnancesurvey.co.uk/documents/product-support/tech-spec/addressbase-technical-specification.pdf"
          ),
          "were critical to matching a ",
          "patient record to an address classified as being a care home. ",
          "These classifications are maintained by Ordnance Survey based on ",
          "information supplied by external agencies ",
          "(e.g. Care Quality Commission), and rely on accurate ",
          "information being supplied to Ordnance Survey. ",
          "To remove potential mismatches a selection of AB building ",
          "classification types were removed from the lookup data, ",
          "such as street records, objects of interest, car parks, ",
          "garages and others. However, although this reduces the ",
          "scope for mismatching there may be other building ",
          "classification types that could also be excluded."
        ),
        tags$li(
          enurl(
            text = "Care Quality Commission (CQC)",
            url = "https://www.cqc.org.uk/"
          ),
          " data was used to supplement ",
          "the AB address information. This increased the number of ",
          "care home addresses within the lookup data and enabled a ",
          "greater number of prescription forms to be labelled as being ",
          "from a care home."
        ),
        # The last sentence gives the impression that being able to label more
        # as CHs was a desired or good outcome. More neutral would be:
        # ...This increased the pool of CH addresses and resulted in more
        # accurate identification of prescriptions for CH residents.
        tags$li(
          "CQC information was sourced through the ",
          enurl(
            text = "cqcr R-package, ",
            url = "https://github.com/evanodell/cqcr"
          ),
          " which in turn acted as a wrapper around the CQC API. ",
          "It was assumed the data collected through the package was ",
          "robust and accurate."
        ),
        # I'm guessing this was removed from the data workflow, as did not see
        # it used.
        tags$li(
          "As with AB, CQC data is also in a ",
          enurl(
            text = "state of continual refinement, ",
            url = "https://www.cqc.org.uk/what-we-do/how-we-use-information/how-we-use-information"
          ),
          "due to the changing nature of property details, and as with AB, ",
          "a snapshot of the CQC data taken at a point in time was used ",
          "for the matching process."
        ),
        tags$li(
          "CQC data also holds additional care home definitions, ",
          "including showing a distinction between nursing and ",
          "residential homes.  The definitions could be extended to AB ",
          "records where the CQC data included a ",
          "Unique Property Reference Number, although this is not ",
          "available for all CQC records."
        )
      )
    })


    output$matching <- renderUI({
      tags$ul(
        br(),
        tags$li(
          "Many of the functions within the ",
          tags$code("{nhsbsaR}"), " and", tags$code("{addressMatchR}"),
          " package are scripted to work specifically with an Oracle dataset. ",
          "Users using a different database architecture may have to edit ",
          "the source code behind these functions, for them to work correctly."
        ),
        tags$li(
          "A Single Line Address (SLA) data field was required for the ",
          "first two matching stages, similar to that found in AB Core. ",
          "The ", tags$code("oracle_merge_strings"),
          "function from the nhsbsaR package aimed to generate a ",
          "Single Line Address (SLA), by combining address components. ",
          "This generated SLA similar, although not identical, ",
          "to that found in AB Core."
        ),
        tags$li(
          "When SLA could not be exact matched, ",
          "they were matched using the individual words (tokens) ",
          "in the patient and lookup addresses. It was possible the same ",
          "lookup token could be matched multiple times against different ",
          "patient address tokens. In some instances, this may have ",
          "exaggerated the similarity between two address strings."
        ),
        tags$li(
          "Where the address strings could not be matched between datasets, ",
          "additional functions were applied to match the remaining records ",
          "to care homes. One such method was to assume that any address, ",
          "sharing a post code with a registered care home and with five or ",
          "more patients aged 65 years receiving prescribing in a single month, ",
          "could be strongly suggested to be a care home, with keyword ",
          "exclusions applied to remove some property types such as ",
          "hospitals etc.  This approach allowed a further 2.7% of ",
          "prescriptions forms to be labelled as belonging to a care home. ",
          "This threshold of five patients, although appropriate in this ",
          "instance, could potentially change in future analyses."
        ),
        tags$li(
          "A final matching stage looked at key words appearing ",
          "in recorded patient addresses (e.g. care home, residential home) ",
          "and allowed a further 4.0% of prescriptions forms were labelled as ",
          "belonging to a care home. The list of keywords for the matches, ",
          "although appropriate in this instance, again could potentially ",
          "change in future analyses."
        ),
        # In above 2 caveats, Maybe just me, but using "allowed" seems a bit as
        # if we were seeking reasons to be able to label prescriptions as being
        # for CH residents. Would a more neutral phrasing be better?
        tags$li(
          "The validation stage of address matching only focused on ",
          "false-positive matches. This was where a patient address ",
          "record was incorrectly labelled as being a care home. ",
          "No work was done to gauge the extent of false-negatives, ",
          "namely patient address records that were care homes although not ",
          "labelled as such."
        ),
        tags$li(
          "The validation estimated the prescription form-level accuracy, ",
          "classifying each prescription form as being from a care home ",
          "address or otherwise, at 99.6%. "
        )
      )
    })


    output$metric <- renderUI({
      tags$ul(
        br(),
        tags$li(
          "A single prescribing status per patient per month was required for ",
          "the ‘Monthly prescribing status of patients aged 65+ who received ",
          "at least one prescription item in a care home during 2020/21’ chart.",
          "There were three types of prescription status. Patients that ",
          "received care home prescribing within a month, those that received ",
          "non-care home prescribing within a month, and those that received ",
          "no prescribing within a month. If a patient received both care home ",
          "and non-care home prescribing within the same month, their status ",
          "was categorised as receiving care home prescribing for that month."
        ),
        tags$li(
          "The monthly increase in the number of patients who received no ",
          "prescribing within a month, was due to patients becoming deceased, ",
          "or receiving no prescribing whilst still alive. Deceased patients ",
          "were not exclusive to this single prescription status. It was ",
          "possible for patients to have both care home and non-care home ",
          "prescribing recorded against their name after their date of death."
        ),
        # So the behaviour is an artifact of having to include all patients from
        # the beginning of the year in each month? Could it not have simply
        # shown 2 categories: Received CH rx Did not receive CH rx?
        #
        # Was showing the distinction between Received non-CH rx and no
        # rx/deceased important? Only asking because the chart generated by
        # using the 3 categories is quite confusing; I puzzled over this for
        # some time until I saw this caveat!
        #
        # "It was possible for patients to have both care home and non-care home
        # prescribing recorded against their name after their date of death."
        # Is this related to the 2nd caveat for Prescriptions data, i.e. timing
        # of submission of rx's to NHSBSA?
        tags$li(
          "A single gender was attributed to each patient, ",
          "to enable gender-level aggregations in ‘Age band and gender of ",
          "estimated older care home patients in England (2020/21)’ chart. ",
          "If a patient gender was either consistently recorded as being ",
          "male or female, they were categorised as such. ",
          "Patients whose gender was recorded differently across ",
          "prescription forms were categorised as having an unknown gender."
        ),
        tags$li(
          "A single age was also attributed to each patient, ",
          "to enable aggregations for multiple charts within the analysis. ",
          "The maximum age recorded across all prescription forms, ",
          "within the reporting period, for a patient was taken as their age."
        ),
        tags$li(
          "Patient geography information was determined using the ",
          "National Statistics Postcode Lookup (NSPL) from the ONS Geoportal. ",
          "A Region, STP or Local Authority could not be attributed to a ",
          "patient address record if they had an unknown postcode, or if ",
          "their postcode was not contained within the NSPL."
        ),
        tags$li(
          "One overarching point to note is that the final analysis was on ",
          "a prescription form-level. This meant we did not have data for ",
          "months in which patients did not receive prescribing."
        ),
        # Is there any data for a patient month if there is no prescribing?!
        # Sorry, I may be missing the point of noting this...
        tags$li(
          "The Per Patient Month (PPM) metrics were calculated by summing ",
          "the total cost, number of items, and number of unique medicines ",
          "for each patient across the period. This total was then divided by ",
          "the number of months in the period where the patient was attributed ",
          "prescribing. For example, if a patient receives 2 prescription ",
          "items in April, 3 in May, and 4 in June, then their number of ",
          "items per patient month metric is calculated as total ",
          "number of items:9 / number of observations(months):3 = 3 ",
          "The mean of these values is then taken across all patients to give ",
          "each per patient month metric. PPM metrics were calculated ",
          "separately for care home and non-care home prescribing. This may ",
          "overstate prescribing for patients who receive multiple months ",
          "prescribing within a single month."
        ),
        # Am I right that when looked at for the whole year, most of that effect
        # will cancel? Only the 'tail end' of the effect would still be left for
        # a few months near the end of the year (from multi-month prescribing
        # near end of period looked at)?
        tags$li(
          "Both the Number of Unique Medicines Per Patient Month and ",
          "Patients on Ten or More Unique Medicines metrics only ",
          "looked at medicines appearing with BNF chapters 1-4 and 6-10, ",
          "in-line with how these metrics are reported in the ",
          "NHSBSA Polypharmacy dashboard."
        ),
        # Could add why Chapter 5 is not included. Also is the Polypharmacy
        # dashboard available publicly? If so needs a link. If not, should it be
        # mentioned?
        tags$li(
          "The mean average has been used to calculate per patient month ",
          "metrics. It should be noted that the distributions are positively ",
          "skewed due to extreme high values for some patients, and the ",
          "median values are lower than the mean."
        ),
        tags$li(
          "In line with the ",
          enurl(
            text = "NHSBSA’s Statistical Disclosure Control Policy ",
            url = "https://www.nhsbsa.nhs.uk/sites/default/files/2020-09/NHSBSA%20-%20Statistical%20Disclosure%20Control%20Protocol.pdf#:~:text=This%20document%20sets%20out%20the%20NHS%20Business%20Services,determined%20on%20a%20case%20by%20case%20risk%20basis."
          ),
          "patient counts between one and four, or item and cost information where ",
          "a patient count between one and four can be inferred, has been redacted ",
          "with “c”. Patient counts have also been omitted from geographical ",
          "breakdowns below national level due to the level of redaction ",
          "that would be applied."
        )
      )
    })
  })
}

## To be copied in the UI
# mod_caveats_ui("caveats_ui_1")

## To be copied in the server
# mod_caveats_server("caveats_ui_1")
