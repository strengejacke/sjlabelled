#' @title Remove value and variable labels from vector or data frame
#' @name remove_all_labels
#'
#' @description This function removes value and variable label attributes
#'                from a vector or data frame. These attributes are typically
#'                added to variables when importing foreign data (see
#'                \code{\link{read_spss}}) or manually adding label attributes
#'                with \code{\link{set_labels}}.
#'
#' @seealso See vignette \href{../doc/intro_sjlabelled.html}{Labelled Data and the sjlabelled-Package},
#'            and \code{\link{copy_labels}} for adding label attributes
#'            (subsetted) data frames.
#'
#' @param x Vector or \code{data.frame} with variable and/or value label attributes
#' @return \code{x} with removed value and variable label attributes.
#'
#' @examples
#' data(efc)
#' str(efc)
#' str(remove_all_labels(efc))
#'
#' @export
remove_all_labels <- function(x) {
  UseMethod("remove_all_labels")
}


#' @export
remove_all_labels.data.frame <- function(x) {
  as.data.frame(lapply(x, FUN = remove_all_labels_helper), stringsAsFactors = FALSE)
}

#' @export
remove_all_labels.list <- function(x) {
  lapply(x, FUN = remove_all_labels_helper)
}

#' @export
remove_all_labels.default <- function(x) {
  remove_all_labels_helper(x)
}

remove_all_labels_helper <- function(x) {
  # remove attributes
  attr(x, "label") <- NULL
  attr(x, "labels") <- NULL

  # unclass, if labelled. labelled class may throw
  # errors / warnings, when not havin label attributes
  if (is_labelled(x)) x <- unclass(x)

  # return var
  x
}
