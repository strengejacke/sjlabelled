#' @title Repair value labels
#' @name tidy_labels
#'
#' @description Duplicated value labels in variables may cause troubles when
#'              saving labelled data, or computing cross tabs (cf.
#'              \code{\link[sjmisc]{flat_table}} or \code{\link[sjPlot]{plot_xtab}}).
#'              \code{tidy_labels()} repairs duplicated value labels by suffixing
#'              them with the associated value.
#'
#' @param sep String that will be used to separate the suffixed value from the
#'            old label when creating the new value label.
#' @param remove Logical, if \code{TRUE}, the original, duplicated value label will
#'            be replaced by the value (i.e. the value is not the suffix of the
#'            value label, but will become the value label itself). The
#'            \code{sep}-argument will be ignored in such cases.
#'
#' @inheritParams add_labels
#'
#' @return \code{x}, with "repaired" (unique) value labels for each variable.
#'
#' @examples
#' library(sjmisc)
#' set.seed(123)
#' x <- set_labels(
#'   sample(1:5, size = 20, replace = TRUE),
#'   labels = c("low" = 1, ".." = 2, ".." = 3, ".." = 4, "high" = 5)
#' )
#' frq(x)
#'
#' z <- tidy_labels(x)
#' frq(z)
#'
#' z <- tidy_labels(x, sep = ".")
#' frq(z)
#'
#' z <- tidy_labels(x, remove = TRUE)
#' frq(z)
#'
#' @export
tidy_labels <- function(x, ..., sep = "_", remove = FALSE) {
  dots <- as.character(match.call(expand.dots = FALSE)$`...`)
  .dat <- .get_dot_data(x, dots)

  if (is.data.frame(x)) {
    # iterate variables of data frame
    for (i in colnames(.dat)) {
      x[[i]] <- tidy_labels_helper(x = .dat[[i]], sep = sep, remove = remove)
    }
  } else {
    x <- tidy_labels_helper(x = .dat, sep = sep, remove = remove)
  }

  x
}


tidy_labels_helper <- function(x, sep, remove) {
  # get value labels from variable. drop unused labels
  labs <-
    get_labels(
      x,
      attr.only = TRUE,
      values = FALSE,
      drop.unused = TRUE,
      drop.na = TRUE
    )

  # no labels? then return...
  if (is.null(labs)) return(x)

  # get values that are associated with labels
  values <- get_values(drop_labels(x), drop.na = TRUE)

  # create table, and check if any value label is duplicated
  duped.val <- names(which(table(labs) > 1))

  # no dupes found? return variable then
  if (isempty(duped.val)) return(x)

  # find position of duplicated labels
  dupes <- lapply(duped.val, function(.x) which(labs == .x))
  dupes <- as.vector(unlist(dupes))

  if (remove) {
    # replace labels with value
    labs[dupes] <- sprintf("%s",values[dupes])
  } else {
    # prefix labels with value
    labs[dupes] <- sprintf("%s%s%s", labs[dupes], sep, values[dupes])
  }


  # set back value labels
  names(values) <- labs
  attr(x, "labels") <- values

  x
}