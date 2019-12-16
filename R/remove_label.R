#' @title Remove variable labels from variables
#' @name remove_label
#'
#' @description Remove variable labels from variables.
#'
#' @seealso \code{\link{set_label}} to manually set variable labels or
#'            \code{\link{get_label}} to get variable labels; \code{\link{set_labels}} to
#'            add value labels, replacing the existing ones (and removing non-specified
#'            value labels).
#'
#' @param x A vector or data frame.
#' @inheritParams as_factor
#'
#' @return \code{x} with removed variable labels
#'
#' @examples
#' data(efc)
#' x <- efc[, 1:5]
#' get_label(x)
#' str(x)
#'
#' x <- remove_label(x)
#' get_label(x)
#' str(x)
#' @export
remove_label <- function(x, ...) {
  dots <- as.character(match.call(expand.dots = FALSE)$`...`)
  .dat <- .get_dot_data(x, dots)

  if (is.data.frame(x)) {
    # iterate variables of data frame
    for (i in colnames(.dat)) {
      attr(x[[i]], "label") <- NULL
    }
  } else {
    attr(x, "label") <- NULL
  }

  x
}
