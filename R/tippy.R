#' @name Custom tippy widget functions
#'
#' @inheritParams tippy::tippy
#'
#' @return a tippy
#' @export
#' @importFrom tippy tippy
#'
#' @noRd
#'
tippy <- function(tooltip,
                  text) {
  tippy::tippy(
    tooltip = tooltip,
    text = text
  )
}
