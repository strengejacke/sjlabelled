#' @title Create a labelled data frame
#' @name lbl_df
#'
#' @description This method wraps a local data frame and adds a \code{lbl_df} class
#'    attribute. Printing a \code{lbl_df}-data frame is comparable
#'    to printing \code{\link[tibble]{tibble}} objects, but the class
#'    information in the output is replaced by the variable label.
#'
#' @param x A data frame.
#'
#' @return \code{x}, with \code{lbl_df} class-attribute.
#'
#' @examples
#' data(efc)
#' library(dplyr)
#'
#' efc %>%
#'   select(e15relat, e16sex, e17age) %>%
#'   slice(1:3) %>%
#'   lbl_df()
#'
#' efc %>%
#'   select(e15relat, e16sex, e17age) %>%
#'   as_label() %>%
#'   set_label(c("Relationship", "Elder's gender", "Elder's age")) %>%
#'   lbl_df()
#'
#' @export
lbl_df <- function(x) {
  # add class attribute, if necessary
  if (!"lbl_df" %in% class(x)) class(x) <- c("lbl_df", class(x))

  x
}

#' @importFrom purrr map
#' @export
format.lbl_df <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  x[] <- purrr::map(x, label_type_sum)
  NextMethod()
}

label_type_sum <- function(x) {
  class(x) <- c("label_type_sum", class(x))
  x
}

#' @export
type_sum.label_type_sum <- function(x) {
  attr(x, "label")
}
