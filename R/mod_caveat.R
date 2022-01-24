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
      "The caveates have grouped into three categories, ",
      "those concerning the underlying prescriptions data, ",
      "the methodology, and the interactive analysis itself. ",
      "Each group of caveats are described within their corresponding tab below."
    ),
    tabsetPanel(
      br(),
      type = "tabs",
      tabPanel(title = "Prescription", htmlOutput(ns("prescription"))),
      tabPanel(title = "Methodology", htmlOutput(ns("methodology"))),
      tabPanel(title = "Analysis", htmlOutput(ns("analysis")))
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
        tags$li(
          "This analysis was focused on older care home patients. ",
          "An older care home patient was categorised by someone aged 65 or above ",
          "receiving prescribing at a care home address. ",
          "Only patient records were considered where the patient age at the time of ",
          "prescribing was 65 or older."
        ),
        tags$li(
          "The analysis only considered patient records from the 2020/21 financial year."
        ),
        tags$li(
          "The final output of the analysis was a prescription form-level dataset, ",
          "labelling each prescription as being from a care home or otherwise. ",
          "All prescription forms with a patient age at the time of prescribing ",
          "being 65 or older was therefore considered for the 2020/21 ",
          "financial year. This amounted to around 258m forms, 16m of which ",
          "were categorised as being from a care home."
        ),
        tags$li(
          "The analysis required that every prescription form had a patient ",
          "address recorded. The matching methodology could not be performed ",
          "without a patient address. Electronic prescribing (ETP) captures an ",
          "address and accounted for 85% of prescribing in the 2020/21 ",
          "financial year. Paper prescribing address information is not captured ",
          "during scanning procedures and does not end up in the NHSBSA Data Warehouse. ",
          "A process was derived to generate patient addresses for these paper forms."
        ),
        tags$li(
          "Using a mix of information from the Patient Demographic Service (PDS) ",
          "and information from ETP across a range of months, a patient address ",
          "was generated for the remaining paper forms, which accounted for ",
          "15% of prescribing in the 2020/21 financial year."
        ),
        tags$li(
          "Section 2.3 of the Workflow and Methodology describes this process ",
          "in full, although in short patient address information for paper ",
          "prescription forms had to derived using a hierarchy of ETP and PDS ",
          "preferences. Although accurate, this could not be considered as ",
          "robust as directly sourced ETP patient address information."
        ),
        tags$li(
          "That said, patient information sourced from electronic prescribing ",
          "also had the possibility of being incorrect."
        ),
        tags$li(
          "For the 2020/21 financial year, patient addresses could be allocated ",
          "for 99.7% of paper prescription forms where the patient’s NHS number ",
          "could be identified, and the patient was aged 65+."
        ),
        tags$li(
          "The analysis was focused on English care home prescribing. ",
          "Patient address information, rather than the prescribing or ",
          "dispensing address information, was used to define which records ",
          "were categorised as such. Patient addresses were classified by ",
          "country by their postcode. If a patient address had an English ",
          "postcode, yet the prescribing address had a known non-English postcode, ",
          "this record would be considered by the analysis. "
        ),
        tags$li(
          "Prescription forms with non-English address information were removed ",
          "from the analysis. However, only", tags$i("known"),
          "non-English addresses were removed. ",
          "This meant a record with an unknown or missing postcode would still ",
          "be considered."
        ),
        tags$li(
          "The matching methodology was conducted on a postcode-level. ",
          "This meant address with an unknown or missing postcode could not be ",
          "Exact or JW matched, although could be Patient Count and Keyword matched."
        ),
        tags$li(
          "The postcode-level matching in the Exact and JW match steps meant ",
          "that if the patient postcode was recorded incorrectly, ",
          "their address would be matched against addresses from the wrong postcode."
        ),
        tags$li(
          "The analysis was based on drugs that were reimbursed by the NHSBSA. ",
          "It did not include items not dispensed, disallowed and returned for ",
          "clarification."
        ),
        tags$li(
          "If a prescription was issued, but not presented for dispensing or ",
          "was not submitted to NHS Prescription Services by the dispenser, ",
          "then it was not included in the data provided."
        ),
        tags$li(
          "Prescription data relates to prescription batches submitted ",
          "to the NHSBSA for payment between April 2020 and March 2021, ",
          "up until the cut off dates. The part month in NHSBSA data relates ",
          "to the dispensing month for which the prescription batch was submitted. ",
          "This is generally but not always the month in which the ",
          "prescription was dispensed. This means that there may be ",
          "dispensing for given patients that has not been submitted ",
          "to the NHSBSA for payment and is therefore not included. ",
          "There may also be prescriptions included for a patient that ",
          "were dispensed prior to the dispensing month."
        ),
        tags$li(
          "Prescription batches which were submitted late are not included in the data."
        ),
        tags$li(
          "Patients may also receive prescription items that have not been ",
          "prescribed to them personally and will not be accounted for. ",
          "This may occur in the case of high volume vaccines such as flu vaccines ",
          "and in the case of bulk prescribing. A bulk prescription is an order ",
          "for two or more patients bearing the name of an institution in which ",
          "at least 20 persons normally reside, 10 or more of whom are ",
          "registered with a particular GP practice. ",
          "Products which can be bought in a pharmacy or ",
          "supermarket can be bulk prescribed, such as Lactulose syrup and ",
          "small volumes of Paracetamol. Prescription only medicines, ",
          "for example antibiotics and blood pressure tablets, ",
          "cannot be issued by bulk prescriptions. ",
          "There is no means of quantifying the extent of this in our data."
        ),
        tags$li(
          "During prescription processing NHS numbers are captured for all ",
          "electronic messages and 92% (???) of paper items.  ",
          "The NHSBSA investigate the accuracy of NHS numbers captured from ",
          "paper forms on an ad-hoc basis (generally every six months). ",
          "The personal details captured (NHS number, date of birth and age) ",
          "are compared against those on the prescription form for a random ",
          "sample of 50,000 prescription forms. ",
          "The NHS number captured typically matches that on the prescription ",
          "form for over 99.9% of forms. The results represent the accuracy for ",
          "all items processed; as such the level of accuracy is ",
          "undetermined for specific medicines, geographies, ",
          "time periods and other factors. "
        ),
        tags$li(
          "The NHS number for electronic messages is that which is ",
          "extracted from practice systems and is assumed to be correct. ",
          "As the utilisation of electronic prescribing increases, ",
          "the coverage and accuracy of this data will increase."
        ),
        tags$li(
          "NHS Prescription Services have a variety of validation streams ",
          "throughout prescription processing to support accurate capture of ",
          "the data. In addition, a retrospective sample is completed in the ",
          "month following reimbursement to identify the accuracy of ",
          "prescription processing information. The check includes ",
          "the accuracy of prescriber, practice and drug information, ",
          "but does not include the personal details of the patient. ",
          "The information is used to populate a number of Key Performance ",
          "Indicators, the results of which are published on the NHSBSA ",
          "website on a monthly basis. "
        ),
        tags$li(
          "The monthly ‘Information Accuracy’ estimates that at least ",
          "XX.X% of all items are processed with no errors relating to the ",
          "product or prescriber/practice. The sample may not be representative ",
          "at a more granular level; as such the level of accuracy is ",
          "undetermined for specific groups such as drugs, geographies, ",
          "time periods etc.  It should also be noted that the identification ",
          "of errors in the accuracy checking sample does not result ",
          "in amendments to data held in NHSBSA systems."
        )
      )
    })


    output$methodology <- renderUI({
      tags$ul(
        style = "font-size: 14pt;",
        tags$li(
          "Ordnance Survey AddressBase (AB) was the foundation of the ",
          "lookup address data, which was matched against patient address ",
          "information. Despite being an accurate and well-developed product, ",
          "it still has occasional address detail error. "
        ),
        tags$li(
          "AB building classifications were critical to matching a patient ",
          "record to an address classified as being a care home. ",
          "It was also possible to have errors in these AB building classifications."
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
          "CQC information was sourced through the cqcr R-package, ",
          "which in turn acted as a wrapper around the CQC API. ",
          "It was assumed the data collected through the package was ",
          "robust and accurate."
        ),
        tags$li(
          "As with AB, CQC data is also in a state of continual refinement, ",
          "due to the changing nature of property details. Once again, ",
          "address information could in practice be incorrect. ",
          "There was also the possibility of errors between CQC care home ",
          "classifications, such as between residential homes and nursing homes."
        ),
        tags$li(
          "Many CQC care homes did not have a UPRN recorded, ",
          "which limited the extent to which CQC care home property ",
          "information could be allocated to AB UPRN."
        ),
        tags$li(
          "Many of the functions within the ",
          tags$i("nhsbsaR"), " and ", tags$i("addressMatchR"),
          " package are scripted to work specifically with an Oracle dataset. ",
          "Users using a different database may have to edit the source code ",
          "behind these functions, in order for them to work. "
        ),
        tags$li(
          "The ", tags$i("oracle_merge_strings"), " function from the ",
          tags$i("nhsbsaR"), " package aimed to generate a Single Line Address (SLA)",
          " similar to that found in AB Core. ",
          "The concatenation of GEO and DPA SLA achieved something close to this, ",
          "although it must be noted that this was not the same as AB Core SLA."
        ),
        tags$li(
          "Although unlikely, the result of the patient address records being ",
          "cleaned using the ", tags$i("tidy_single_line_address"),
          " function from the ", tags$i("nhsbsaR"), " package, may have ",
          "inadvertently affected how the address record was matched."
        ),
        tags$li(
          "Within the token-level matching performed by the ",
          tags$i("calc_match_addresses"), " contained within the NHSBSA ",
          tags$i("addressMatchR"), " package, the same lookup token could be ",
          "matched multiple times against different patient address tokens. ",
          "In some instances, this may have exaggerated the similarity ",
          "between two address strings."
        ),
        tags$li(
          "Patient count matches were conducted when five of more patients ",
          "aged 65 or over received prescribing an address with a ",
          "postcode where there was a care home. This threshold of five patients, ",
          "although appropriate in this instance, could potentially change in ",
          "future analyses."
        ),
        tags$li(
          "The list of keywords for the Keyword matches, ",
          "although appropriate in this instance, again could potentially ",
          "change in future analyses."
        ),
        tags$li(
          "The validation stage of the analysis only focused on ",
          "false-positive matches. This was where a patient address record was ",
          "labelled as being a care home, although was not actually a care home. ",
          "No work was done to gauge the extent of false-negatives, ",
          "namely patient address records that were care homes although not",
          "labelled as such."
        ),
        tags$li(
          "The validation estimated the prescription form-level accuracy, ",
          "classifying each prescription form as being from a care home address ",
          "or otherwise, at 99.5%. Despite this, there will still be instances ",
          "where a prescription form was classified as being from a care home ",
          "address yet not actually a care home, and vice-versa."
        )
      )
    })

    output$analysis <- renderUI({
      tags$ul(
        style = "font-size: 14pt;",
        tags$li(
          "On overarching point to note is that the final analysis was on a ",
          "prescription form-level. This meant we did not have data for months ",
          "in which potential care home residents did not receive prescribing."
        ),
        tags$li(
          "Patient address records with known non-English postcodes were ",
          "removed from the analysis. This did however mean that some patient ",
          "address records were present with an unknown postcode, ",
          "and excluded from certain analyses."
        ),
        tags$li(
          "The patient prescribing status required Date of Death (DOD) ",
          "information sourced from the CIP table. This was required for the ",
          "‘Monthly prescribing status of patients aged 65+ who received at ",
          "least one prescription item in a care home during 2020/21’ chart. ",
          "The DOD from the CIP table is not always accurate and is in a ",
          "continual state of refinement."
        ),
        tags$li(
          "A single prescribing status per patient per month was also required ",
          "for the ‘Monthly prescribing status of patients aged 65+ who received ",
          "at least one prescription item in a care home during 2020/21’ chart. ",
          "When a patient had a mixed prescribing status within a month with at ",
          "least one care home classified prescription form, their status was ",
          "taken as being a care home resident for that month."
        ),
        tags$li(
          "A single gender needed to attributed to each patient, ",
          "to enable gender-level aggregations in order to create the ",
          "‘Age band and gender of estimated older care home patients in ",
          "England (2020/21)’ chart. If a patient gender was either ",
          "consistently recorded as being Male or Female, they were ",
          "categorised as such. Patients whose gender was recorded differently ",
          "across prescription forms were categorised as having an unknown gender.  "
        ),
        tags$li(
          "A single age also needed to be attributed to each patient, ",
          "to enable aggregations for multiple charts within the analysis. ",
          "The maximum age recorded across all prescription forms for a ",
          "patient was taken as their age. "
        ),
        tags$li(
          "Patient geography information was determined using the National ",
          "Statistics Postcode Lookup (NSPL) from the ONS Geoportal. ",
          "A patient Region, STP or Local Authority could not be attributed to ",
          "a patient address record if they had an unknown postcode, or ",
          "if their postcode was not contained within the NSPL."
        ),
        tags$li(
          "The drug cost (or cost per patient) metric was calculated by first ",
          "calculating the cost of each month in which a patient received ",
          "prescribing, and then taking the mean of these values. ",
          "This means the Drug Cost value would in some instances be inaccurate, ",
          "such as with patients who receive repeat prescribing. ",
          "For example, a patient receiving quarterly prescribing ",
          "would have a mean calculated from the cost of the four months ",
          "in which they received prescribing, as opposed to this cost ",
          "averaged across a twelve-month period."
        ),
        tags$li(
          "Caveats around the number of prescription items metric were ",
          "similar to those above. Namely, the mean number of monthly ",
          "prescriptions items that a patient receives, ",
          "is only calculated from the months in which they receive prescribing, ",
          "as opposed to this number averaged across a twelve-month period."
        ),
        tags$li(
          "The number of unique medicines metric was calculated using the ",
          "number of unique medicines for all patients, regardless whether ",
          "or not they passed away during the analysis time period. ",
          "Patients who passed away within the first few months of the ",
          "analysis time period could therefore skew this calculation. ",
          "This same consideration also applied to the patients on ten or more ",
          "unique medicines metric. "
        ),
        tags$li(
          "Both the number of unique medicines and patients on ten or more ",
          "unique medicines metrics only looked at medicines appearing with ",
          "BNF chapters 1-4 and 6-10. This was in-line with the ",
          "NHSBSA Polypharmacy dashboard."
        ),
        tags$li(
          "Drugs from all BNF chapters could be explored in the final ",
          "dumbbell chart within the interactive analysis.  "
        )
      )
    })
  })
}

## To be copied in the UI
# mod_caveat_ui("caveat_ui_1")

## To be copied in the server
# mod_caveat_server("caveat_ui_1")
