#' @title Add, replace or remove value labels of variables
#' @name add_labels
#'
#' @description These functions add, replace or remove value labels to or from variables.
#'
#' @seealso \code{\link{set_label}} to manually set variable labels or
#'            \code{\link{get_label}} to get variable labels; \code{\link{set_labels}} to
#'            add value labels, replacing the existing ones (and removing non-specified
#'            value labels).
#'
#' @param x A vector or data frame.
#' @param labels \describe{
#'          \item{For \code{add_labels()}}{A named (numeric) vector of labels
#'          that will be added to \code{x} as label attribute.}
#'          \item{For \code{remove_labels()}}{Either a numeric vector, indicating
#'          the position of one or more label attributes that should be removed;
#'          a character vector with names of label attributes that should be
#'          removed; or a \code{\link[haven:tagged_na]{tagged_na()}} to remove the labels
#'          from specific NA values.}
#'          }
#'
#' @inheritParams as_factor
#'
#' @return \code{x} with additional or removed value labels. If \code{x}
#'           is a data frame, the complete data frame \code{x} will be returned,
#'           with removed or added to variables specified in \code{...};
#'           if \code{...} is not specified, applies to all variables in the
#'           data frame.
#'
#' @details \code{add_labels()} adds \code{labels} to the existing value
#'          labels of \code{x}, however, unlike \code{\link{set_labels}}, it
#'          does \emph{not} remove labels that were \emph{not} specified in
#'          \code{labels}. \code{add_labels()} also replaces existing
#'          value labels, but preserves the remaining labels.
#'          \cr \cr
#'          \code{remove_labels()} is the counterpart to \code{add_labels()}.
#'          It removes labels from a label attribute of \code{x}.
#'          \cr \cr
#'          \code{replace_labels()} is an alias for \code{add_labels()}.
#'
#' @examplesIf require("dplyr") && require("haven")
#' # add_labels()
#' data(efc)
#' get_labels(efc$e42dep)
#'
#' x <- add_labels(efc$e42dep, labels = c(`nothing` = 5))
#' get_labels(x)
#'
#' if (require("dplyr")) {
#'   x <- efc %>%
#'     # select three variables
#'     dplyr::select(e42dep, c172code, c161sex) %>%
#'     # only add new label to two of those
#'     add_labels(e42dep, c172code, labels = c(`nothing` = 5))
#'   # see data frame, with selected variables having new labels
#'   get_labels(x)
#' }
#'
#' x <- add_labels(efc$e42dep, labels = c(`nothing` = 5, `zero value` = 0))
#' get_labels(x, values = "p")
#'
#' # replace old value labels
#' x <- add_labels(
#'   efc$e42dep,
#'   labels = c(`not so dependent` = 4, `lorem ipsum` = 5)
#' )
#' get_labels(x, values = "p")
#'
#' # replace specific missing value (tagged NA)
#' if (require("haven")) {
#'   x <- labelled(c(1:3, tagged_na("a", "c", "z"), 4:1),
#'                 c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
#'                   "Refused" = tagged_na("a"), "Not home" = tagged_na("z")))
#'   # get current NA values
#'   x
#'   # tagged NA(c) has currently the value label "First", will be
#'   # replaced by "Second" now.
#'   replace_labels(x, labels = c("Second" = tagged_na("c")))
#' }
#'
#'
#' # remove_labels()
#'
#' x <- remove_labels(efc$e42dep, labels = 2)
#' get_labels(x, values = "p")
#'
#' x <- remove_labels(efc$e42dep, labels = "independent")
#' get_labels(x, values = "p")
#'
#' if (require("haven")) {
#'   x <- labelled(c(1:3, tagged_na("a", "c", "z"), 4:1),
#'                 c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
#'                   "Refused" = tagged_na("a"), "Not home" = tagged_na("z")))
#'   # get current NA values
#'   get_na(x)
#'   get_na(remove_labels(x, labels = tagged_na("c")))
#' }
#' @export
add_labels <- function(x, ..., labels) {

  # check for valid value. value must be a named vector
  if (is.null(labels)) stop("`labels` is NULL.", call. = FALSE)
  if (is.null(names(labels))) stop("`labels` must be a named vector.", call. = FALSE)

  # evaluate arguments, generate data
  dots <- as.character(match.call(expand.dots = FALSE)$`...`)
  .dat <- .get_dot_data(x, dots)

  if (is.data.frame(x)) {
    # iterate variables of data frame
    for (i in colnames(.dat)) {
      x[[i]] <- add_labels_helper(.dat[[i]], value = labels)
    }
  } else {
    x <- add_labels_helper(.dat, value = labels)
  }

  x
}

add_labels_helper <- function(x, value) {
  # get current labels of `x`
  current.labels <- get_labels(
    x,
    attr.only = TRUE,
    values = "n",
    non.labelled = FALSE,
    drop.na = TRUE
  )

  current.na <- NULL

  # if we had already labels, append new ones
  if (!is.null(current.labels)) {
    # remove multiple value labels
    doubles <- names(current.labels) %in% as.character(value)

    # switch value and names attribute, since get_labels()
    # returns the values as names, and the value labels
    # as "vector content"
    val.switch <- as.numeric(names(current.labels))
    names(val.switch) <- as.character(current.labels)

    # update all labels
    all.labels <- c(val.switch[!doubles], value)

    # tell user
    if (any(doubles)) {
      message(sprintf(
        "label '%s' was replaced with new value label.\n",
        current.labels[doubles]
      ))
    }
  } else {
    all.labels <- value
  }

  if (requireNamespace("haven", quietly = TRUE)) {
    # get current NA values - requires haven
    current.na <- get_na(x)
    # replace tagged NA
    if (any(haven::is_tagged_na(value))) {
      # get tagged NAs
      value_tag <- haven::na_tag(value)[haven::is_tagged_na(value)]
      cna_tag <- haven::na_tag(current.na)

      # find matches (replaced NA), i.e. see if 'x' has any
      # tagged NA values that match the tagged NA specified in 'value'
      doubles <- na.omit(match(value_tag, cna_tag))

      # tell user if we found any tagged NA, and that these will be replaced
      if (any(doubles)) {
        message(sprintf(
          "tagged NA '%s' was replaced with new value label.\n",
          names(current.na)[doubles]
        ))
      }

      # remove multiple tagged NA
      current.na <- current.na[-doubles]
    }
  }

  # sort labels by values
  all.labels <- all.labels[order(as.numeric(all.labels))]

  # add NA
  if (!is.null(current.na)) all.labels <- c(all.labels, current.na)

  # set back value labels
  attr(x, "labels") <- all.labels

  x
}


#' @rdname add_labels
#' @export
replace_labels <- function(x, ..., labels) {
  add_labels(x = x, ..., labels = labels)
}
