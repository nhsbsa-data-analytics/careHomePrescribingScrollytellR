#' caveat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_caveat_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Caveats"),
    br(),
    p(
      "The caveats are grouped into three categories, ",
      "those concerning the underlying prescriptions data, ",
      "the linking methodology, and the interactive analysis itself. ",
      "Each group of caveats are described within their corresponding tab below."
    ),
    tabsetPanel(
      type = "tabs",
      tabPanel(
        title = "Prescriptions data",
        tags$style("li a { font-weight: bold; font-size: 20px}"),
        htmlOutput(ns("prescription"))
      ),
      tabPanel(title = "Patient data", htmlOutput(ns("patient"))),
      tabPanel(title = "Lookup address", htmlOutput(ns("address"))),
      tabPanel(title = "Address matching", htmlOutput(ns("matching"))),
      tabPanel(title = "Metrics & Charts", htmlOutput(ns("metric")))
    )
  )
}

#' caveat Server Functions
#'
#' @noRd
mod_caveat_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$prescription <- renderUI({
      tags$ul(
        style = "font-size: 14pt;",
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
          "and reimbursement prescriptions issued and dispensed in prisons, ",
          "hospitals and private prescriptions"
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
          "Further Prescription Processing Information Accuracy can be found ",
          enurl(
            text = "here",
            url = "https://www.nhsbsa.nhs.uk/pharmacies-gp-practices-and-appliance-contractors/payments-and-pricing/how-we-process-prescriptions"
          )
        )
      )
    })

    output$patient <- renderUI({
      tags$ul(
        style = "font-size: 14pt;",
        br(),
        tags$li(
          "The analysis focuses on prescriptions for older patients ",
          "aged 65 years or above at the time of prescribing. ",
          "Patient age was determined using a mixture of patient ",
          "information from prescription forms and ",
          "Patient Demographic Service (PDS). Further details on ",
          "the process of patient age determination can be found ",
          enurl(
            text = "here",
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
          "AB is a product that is in a continual state of refinement. ",
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
          "Only patients with a PDS verified NHS ",
          "number were included in the analysis results."
        ),
        tags$li(
          "NHS numbers are captured for 100% electronic prescription ",
          "messages. The estimated NHS number capture rate for paper ",
          "prescription forms is 83.7%."
        ),
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
          "from the Patient Demographic Service (PDS) and electronic ",
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
        style = "font-size: 14pt;",
        br(),
        tags$li(
          "Ordnance Survey AddressBase (AB) was the foundation of the ",
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
          "AB building classifications were critical to matching a ",
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
          "Care Quality Commission (CQC) data was used to supplement ",
          "the AB address information. This increased the number of ",
          "care home addresses within the lookup data and enabled a ",
          "greater number of prescription forms to be labelled as being ",
          "from a care home."
        ),
        tags$li(
          "CQC information was sourced through the cqcr R-package, ",
          "which in turn acted as a wrapper around the CQC API. ",
          "It was assumed the data collected through the package was ",
          "robust and accurate."
        ),
        tags$li(
          "As with AB, CQC data is also in a state of continual refinement, ",
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
        style = "font-size: 14pt;",
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
          "first two matching stages, siilar to that found in AB Core. ",
          "The ", tags$code("oracle_merge_strings"),
          "function from the nhsbsaR package aimed to generate a ",
          "Single Line Address (SLA), by combining address components. ",
          "This generated SLA is similar, although not identical, ",
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
        style = "font-size: 14pt;",
        br(),
        tags$li(
          "The patient prescribing status required Date of Death (DOD) ",
          "information sourced from PDS data. These data are based on either ",
          "formal and informal death statuses, although within the data ",
          "available to NHSBSA it is not possible to identify if the date of ",
          "death is informal, as initially reported by a healthcare ",
          "organisation, or formal following confirmation via the ",
          "General Registry Office. PDS data is the national electronic ",
          "database of NHS patient details such as name address, gender, ",
          "date of birth and NHS Number, which is maintained by NHS Digital."
        ),
        tags$li(
          "A single prescribing status (care home or non-care home) ",
          "per patient per month was required for the ‘Monthly prescribing ",
          "status of patients aged 65+ who received at least one prescription ",
          "item in a care home during 2020/21’ chart. When a patient had a ",
          "mixed prescribing status within a month, with at least one care ",
          "home classified prescription form, their status was taken as ",
          "being a care home patient for that month."
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
          "A single gender was attributed to each patient, ",
          "to enable gender-level aggregations in ‘Age band and gender of ",
          "estimated older care home patients in England (2020/21)’ chart. ",
          "If a patient gender was either consistently recorded as being ",
          "Male or Female, they were categorised as such. ",
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
          "On overarching point to note is that the final analysis was on ",
          "a prescription form-level. This meant we did not have data for ",
          "months in which patients did not receive prescribing."
        ),
        tags$li(
          "The per Patient per Month metrics were calculated by first ",
          "calculating the cost/items/number of unique medicines for each ",
          "month in which a patient received prescribing, and then taking ",
          "the mean of these values. This was calculated monthly, as a ",
          "patient can be in a care home anywhere between one and twelve ",
          "months of a year. This may overstate prescribing for patients ",
          "who receive multiple months prescribing within a single month."
        ),
        tags$li(
          "Both the Number of Unique Medicines Per Patient Month and ",
          "Patients on Ten or More Unique Medicines metrics only ",
          "looked at medicines appearing with BNF chapters 1-4 and 6-10, ",
          "in-line with how these metrics are reported in the ",
          "NHSBSA Polypharmacy dashboard."
        ),
        tags$li(
          "The mean average has been used to calculate per patient month ",
          "metrics. It should be noted that the distributions are positively ",
          "skewed due to extreme high values for some patients, and the ",
          "median values are lower than the mean."
        )
      )
    })
  })
}

## To be copied in the UI
# mod_caveat_ui("caveat_ui_1")

## To be copied in the server
# mod_caveat_server("caveat_ui_1")
