#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %||% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList


#' Define tooltip text
#'
#' @noRd
tooltip_text <- list(
  # Older
  older = "<strong>Older patients </strong> are patients aged 65+ at the time of prescribing.",
  # Care home vs non-care home
  care_home = "<strong>Older care home patients </strong> are patients aged 65+ who received their prescription whilst living in a care home at the time of prescribing.",
  non_care_home = "<strong>Older non-care home patients </strong> are patients aged 65+ who received their prescription whilst not living in a care home at the time of prescribing.",
  residential_home = "A <strong>residential home</strong> is a care home where a qualified nurse is not provided.",
  nursing_home = "A <strong>nursing home</strong> is a care home where a qualified nurse is provided to ensure that the full needs of the person using the service are met.",
  # Metrics
  items = "Calculated as the average <strong>number of prescription items</strong> per patient per month.<br><br>A count of the number of times a product, such as a drug or appliance, appears on a prescription form. It does not account for dosage or quantity prescribed.<br><br>For example, a patient could receive 100 x 50mg tablets as an item and another could receive 7 x 5 mg tablets as an item. Both would be counted as 1 item",
  cost = "Calculated as the average total <strong>price reimbursed for dispensed drugs</strong> per patient per month.<br><br>It relates solely to the cost of the drugs, in the quantity prescribed on a prescription form. It does not include any additional fees or discounts that were paid to the dispensing contractors.",
  unique_medicines = "Calculated as the average <strong>number of unique medicines</strong> per patient per month.<br><br>A unique medicine is defined as a medicine prescribed with the same chemical substance descriptor in BNF* Sections 1 to 4 and 6 to 10 whether it be different formulations (presentations) or different strengths. Medicines with the same chemical substance descriptor would be counted as one (single) unique product e.g. Warfarin 1mg, 3mg and 5mg tablets.",
  ten_or_more_unique_medicines = "Calculated as the average <strong>percentage of patients prescribed ten or more unique medicines</strong> per month.<br><br>A unique medicine is defined as a medicine prescribed with the same chemical substance descriptor in BNF* Sections 1 to 4 and 6 to 10 whether it be different formulations (presentations) or different strengths. Medicines with the same chemical substance descriptor would be counted as one (single) unique product e.g. Warfarin 1mg, 3mg and 5mg tablets.",
  bnf_code = "A <strong>BNF code</strong> is a fifteen character hierarchical code used to identify medicines based on the <a href='https://www.bnf.org/products/bnf-online/' target='_blank'> <span style='font-size: 14px;'> British National Formulary (BNF)</span></a> classifications."
)
