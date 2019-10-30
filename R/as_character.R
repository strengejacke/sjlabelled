#' @rdname as_label
#' @export
as_character <- function(x, ...) {
  UseMethod("as_character")
}


#' @export
as_character.default <- function(x, add.non.labelled = FALSE, prefix = FALSE, var.label = NULL, drop.na = TRUE, drop.levels = FALSE, ...) {
  as_character_helper(x, add.non.labelled, prefix, var.label, drop.na, drop.levels)
}


#' @rdname as_label
#' @export
as_character.data.frame <- function(x, ..., add.non.labelled = FALSE, prefix = FALSE, var.label = NULL, drop.na = TRUE, drop.levels = FALSE, keep.labels = FALSE) {
  dots <- sapply(eval(substitute(alist(...))), deparse)
  .dat <- .get_dot_data(x, dots)

  # iterate variables of data frame
  for (i in colnames(.dat)) {
    x[[i]] <- as_character_helper(.dat[[i]], add.non.labelled, prefix, var.label, drop.na, drop.levels)
  }

  x
}

as_character_helper <- function(x, add.non.labelled = FALSE, prefix = FALSE, var.label = NULL, drop.na = TRUE, drop.levels = FALSE) {
  # get variable labels
  vl <- get_label(x)

  # to character
  x <- as.character(as_label_helper(x, add.non.labelled, prefix, var.label, drop.na, drop.levels, keep.labels = FALSE))

  # set back variable labels, if any
  if (!is.null(vl)) x <- set_label(x, vl)

  x
}
