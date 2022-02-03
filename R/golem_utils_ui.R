#' Turn an R list into an HTML list
#'
#' @param list An R list
#' @param class a class for the list
#'
#' @return an HTML list
#' @noRd
#'
#' @examples
#' list_to_li(c("a", "b"))
#' @importFrom shiny tags tagAppendAttributes tagList
list_to_li <- function(list, class = NULL) {
  if (is.null(class)) {
    tagList(
      lapply(
        list,
        tags$li
      )
    )
  } else {
    res <- lapply(
      list,
      tags$li
    )
    res <- lapply(
      res,
      function(x) {
        tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    tagList(res)
  }
}
#' Turn an R list into corresponding HTML paragraph tags
#'
#' @param list an R list
#' @param class a class for the paragraph tags
#'
#' @return An HTML tag
#' @noRd
#'
#' @examples
#' list_to_p(c("This is the first paragraph", "this is the second paragraph"))
#' @importFrom shiny tags tagAppendAttributes tagList
#'
list_to_p <- function(list, class = NULL) {
  if (is.null(class)) {
    tagList(
      lapply(
        list,
        tags$p
      )
    )
  } else {
    res <- lapply(
      list,
      tags$p
    )
    res <- lapply(
      res,
      function(x) {
        tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    tagList(res)
  }
}

#' @importFrom shiny tags tagAppendAttributes tagList
named_to_li <- function(list, class = NULL) {
  if (is.null(class)) {
    res <- mapply(
      function(x, y) {
        tags$li(
          HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list,
      names(list),
      SIMPLIFY = FALSE
    )
    tagList(res)
  } else {
    res <- mapply(
      function(x, y) {
        tags$li(
          HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list,
      names(list),
      SIMPLIFY = FALSE
    )
    res <- lapply(
      res,
      function(x) {
        tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    tagList(res)
  }
}

#' Remove a tag attribute
#'
#' @param tag the tag
#' @param ... the attributes to remove
#'
#' @return a new tag
#' @noRd
#'
#' @examples
#' a <- shiny::tags$p(src = "plop", "pouet")
#' tagRemoveAttributes(a, "src")
tagRemoveAttributes <- function(tag, ...) {
  attrs <- as.character(list(...))
  for (i in seq_along(attrs)) {
    tag$attribs[[attrs[i]]] <- NULL
  }
  tag
}

#' Hide or display a tag
#'
#' @param tag the tag
#'
#' @return a tag
#' @noRd
#'
#' @examples
#' ## Hide
#' a <- shiny::tags$p(src = "plop", "pouet")
#' undisplay(a)
#' b <- shiny::actionButton("go_filter", "go")
#' undisplay(b)
#' @importFrom shiny tagList
undisplay <- function(tag) {
  # if not already hidden
  if (
    !is.null(tag$attribs$style) &&
      !grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- paste(
      "display: none;",
      tag$attribs$style
    )
  } else {
    tag$attribs$style <- "display: none;"
  }
  tag
}

#' @importFrom shiny tagList
display <- function(tag) {
  if (
    !is.null(tag$attribs$style) &&
      grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- gsub(
      "(\\s)*display:(\\s)*none(\\s)*(;)*(\\s)*",
      "",
      tag$attribs$style
    )
  }
  tag
}

#' Hide an elements by calling jquery hide on it
#'
#' @param id the id of the element to hide
#'
#' @noRd
#'
#' @importFrom shiny tags
jq_hide <- function(id) {
  tags$script(sprintf("$('#%s').hide()", id))
}

#' Add a red star at the end of the text
#'
#' Adds a red star at the end of the text
#' (for example for indicating mandatory fields).
#'
#' @param text the HTLM text to put before the red star
#'
#' @return an html element
#' @noRd
#'
#' @examples
#' with_red_star("Enter your name here")
#' @importFrom shiny tags HTML
with_red_star <- function(text) {
  shiny::tags$span(
    HTML(
      paste0(
        text,
        shiny::tags$span(
          style = "color:red", "*"
        )
      )
    )
  )
}



#' Repeat tags$br
#'
#' @param times the number of br to return
#'
#' @return the number of br specified in times
#' @noRd
#'
#' @examples
#' rep_br(5)
#' @importFrom shiny HTML
rep_br <- function(times = 1) {
  HTML(rep("<br/>", times = times))
}

#' Create an url
#'
#' @param url the URL
#' @param text the text to display
#'
#' @return an a tag
#' @noRd
#'
#' @examples
#' enurl("https://www.thinkr.fr", "ThinkR")
#' @importFrom shiny tags
enurl <- function(url, text) {
  tags$a(
    href = url,
    text,
    target = "_blank",
    style = "text-decoration: underline"
  )
}

#' Columns wrappers
#'
#' These are convenient wrappers around
#' `column(12, ...)`, `column(6, ...)`, `column(4, ...)`...
#'
#' @noRd
#'
#' @importFrom shiny column
col_12 <- function(...) {
  column(12, ...)
}

#' @importFrom shiny column
col_10 <- function(...) {
  column(10, ...)
}

#' @importFrom shiny column
col_9 <- function(...) {
  column(9, ...)
}

#' @importFrom shiny column
col_8 <- function(...) {
  column(8, ...)
}

#' @importFrom shiny column
col_7 <- function(...) {
  column(7, ...)
}

#' @importFrom shiny column
col_6 <- function(...) {
  column(6, ...)
}

#' @importFrom shiny column
col_5 <- function(...) {
  column(5, ...)
}

#' @importFrom shiny column
col_4 <- function(...) {
  column(4, ...)
}

#' @importFrom shiny column
col_3 <- function(...) {
  column(3, ...)
}

#' @importFrom shiny column
col_2 <- function(...) {
  column(2, ...)
}

#' @importFrom shiny column
col_1 <- function(...) {
  column(1, ...)
}



#' Make the current tag behave like an action button
#'
#' Only works with compatible tags like button or links
#'
#' @param tag Any compatible tag.
#' @param inputId Unique id. This will host the input value to be used
#' on the server side.
#'
#' @return The modified tag with an extra id and the action button class.
#' @noRd
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   link <- a(href = "#", "My super link", style = "color: lightblue;")
#'
#'   ui <- fluidPage(
#'     make_action_button(link, inputId = "mylink")
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$mylink, {
#'       showNotification("Pouic!")
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
make_action_button <- function(tag, inputId = NULL) {
  # some obvious checks
  if (!inherits(tag, "shiny.tag")) stop("Must provide a shiny tag.")
  if (!is.null(tag$attribs$class)) {
    if (grep("action-button", tag$attribs$class)) {
      stop("tag is already an action button")
    }
  }
  if (is.null(inputId) && is.null(tag$attribs$id)) {
    stop("tag does not have any id. Please use inputId to be able to
           access it on the server side.")
  }

  # handle id
  if (!is.null(inputId)) {
    if (!is.null(tag$attribs$id)) {
      warning(
        paste(
          "tag already has an id. Please use input$",
          tag$attribs$id,
          "to access it from the server side. inputId will be ignored."
        )
      )
    } else {
      tag$attribs$id <- inputId
    }
  }

  # handle class
  if (is.null(tag$attribs$class)) {
    tag$attribs$class <- "action-button"
  } else {
    tag$attribs$class <- paste(tag$attribs$class, "action-button")
  }
  # return tag
  tag
}


#' Define tooltip text
#'
#' @noRd
tooltip_text <- list(
  # Older
  older = tags$div(
    class = "nhsuk-u-font-size-14",
    tags$strong("Older patients"), "are patients aged 65+ at the time of prescribing."
  ),
  # Care home vs non-care home
  care_home = tags$div(
    class = "nhsuk-u-font-size-14",
    tags$strong("Older care home patients"), "are patients aged 65+ who ",
    "received their prescription whilst living in a care home at the time of ",
    "prescribing.",
  ),
  non_care_home = tags$div(
    class = "nhsuk-u-font-size-14",
    tags$strong("Older non-care home patients"), "are patients aged 65+ who ",
    "received their prescription whilst not living in a care home at the time ",
    "of prescribing.",
  ),
  # Residential vs nursing
  residential_home = tags$div(
    class = "nhsuk-u-font-size-14",
    "A", tags$strong("residential home"), "is a care home where a qualified ",
    "nurse is not provided.",
  ),
  nursing_home = tags$div(
    class = "nhsuk-u-font-size-14",
    "A", tags$strong("nursing home"), "is a care home where a qualified nurse ",
    "is provided to ensure that the full needs of the person using the ",
    "service are met.",
  ),
  # Metrics
  items = tags$div(
    class = "nhsuk-u-font-size-14",
    "Calculated as the average", tags$strong("number of prescription items"),
    "per patient month. A count of the number of times a product, such as a ",
    "drug or appliance, appears on a prescription form. It does not account ",
    "for dosage or quantity prescribed. For example, a patient could receive ",
    "100 x 50mg tablets as an item and another could receive 7 x 5 mg ",
    "tablets as an item. Both would be counted as 1 item"
  ),
  cost = tags$div(
    class = "nhsuk-u-font-size-14",
    "Calculated as the average",
    tags$b("total price reimbursed for dispensed drugs"),
    "per patient month. It relates solely to the cost of the drugs, in the ",
    "quantity prescribed on a prescription form. It does not include any ",
    "additional fees or discounts that were paid to the dispensing ",
    "contractors."
  ),
  unique_medicines = tags$div(
    class = "nhsuk-u-font-size-14",
    "Calculated as the average", tags$b("number of unique medicines"), "per ",
    "patient per month. A unique medicine is defined as a medicine prescribed ",
    "with the same chemical substance descriptor in BNF Sections 1 to 4 and 6 ",
    "to 10 whether it be different formulations (presentations) or different ",
    "strengths. Medicines with the same chemical substance descriptor would ",
    "be counted as one (single) unique product e.g. Warfarin 1mg, 3mg and 5mg ",
    "tablets."
  ),
  ten_or_more_unique_medicines = tags$div(
    class = "nhsuk-u-font-size-14",
    "Calculated as the average",
    tags$strong(
      "percentage of patients prescribed ten or more unique medicines"
    ), "per month. A unique medicine is defined as a medicine prescribed with ",
    "the same chemical substance descriptor in BNF Sections 1 to 4 and 6 to ",
    "10 whether it be different formulations (presentations) or different ",
    "strengths. Medicines with the same chemical substance descriptor would ",
    "be counted as one (single) unique product e.g. Warfarin 1mg, 3mg and 5mg ",
    "tablets."
  ),
  bnf_code = tags$div(
    class = "nhsuk-u-font-size-14",
    "A", tags$strong("BNF code"), "is a fifteen character hierarchical code ",
    "used to identify medicines based on the ",
    enurl(
      text = "British National Formulary (BNF)",
      url = "https://www.bnf.org/products/bnf-online/"
    ),
    " classifications."
  )
)


# UNCOMMENT AND USE
#
# usethis::use_package("markdown")
# usethis::use_package("rmarkdown")
#
# To use this part of the UI
#
#' #' Include Content From a File
#' #'
#' #' Load rendered RMarkdown from a file and turn into HTML.
#' #'
#' #' @rdname includeRMarkdown
#' #' @export
#' #'
#' #' @importFrom rmarkdown render
#' #' @importFrom markdown markdownToHTML
#' #' @importFrom shiny HTML
#' includeRMarkdown <- function(path){
#'
#'   md <- tempfile(fileext = '.md')
#'
#'   on.exit(unlink(md),add = TRUE)
#'
#'   rmarkdown::render(
#'     path,
#'     output_format = 'md_document',
#'     output_dir = tempdir(),
#'     output_file = md,quiet = TRUE
#'     )
#'
#'   html <- markdown::markdownToHTML(md, fragment.only = TRUE)
#'
#'   Encoding(html) <- "UTF-8"
#'
#'   return(HTML(html))
#' }
