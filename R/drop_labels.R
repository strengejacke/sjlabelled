#' @rdname zap_labels
#' @export
drop_labels <- function(x, ..., drop.na = TRUE) {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  if (is.data.frame(x)) {
    # iterate variables of data frame
    for (i in colnames(.dat)) {
      x[[i]] <- drop_labels_helper(.dat[[i]], drop.na = drop.na)
    }
    # coerce to tibble
    x <- tibble::as_tibble(x)
  } else {
    x <- drop_labels_helper(.dat, drop.na = drop.na)
  }

  x
}

drop_labels_helper <- function(x, drop.na) {
  # retrieve named labels
  tidy.labels <- attr(x, "labels", exact = T)
  tidy.labels <- tidy.labels[!haven::is_tagged_na(tidy.labels)]

  # return x, if no attribute
  if (is.null(tidy.labels)) return(x)

  # all missing in variable?
  if (all(is.na(x))) return(x)

  # remove labels with no values in data
  tidy.labels <- tidy.labels[get_values(x, drop.na = drop.na) %in% names(table(x))]

  # check if tidy labels is empty - then remove everything
  if (isempty(tidy.labels)) tidy.labels <- ""

  # check if user wants to keep labels for NA values or not.
  if (!drop.na) {
    current.na <- get_na(x)
    if (!is.null(current.na) && length(current.na) > 0)
      tidy.labels <- c(tidy.labels, current.na)
  }

  # set back labels
  if (isempty(tidy.labels)) {
    attr(x, "labels") <- NULL
  } else {
    labs <- names(tidy.labels)

    # check if all label-values are numeric, and if so, convert to numeric
    if (is.num.chr(labs)) labs <- as.numeric(labs)

    # set back labels attribute
    names(labs) <- tidy.labels
    attr(x, "labels") <- labs

    # if labels, e.g. due to taggend NA, are no longer of same
    # type as labelled vector, remove labelled class attribute -
    # else, haven will throw errors
    if (inherits(x, "labelled") && typeof(x) != typeof(labs))
      x <- unclass(x)
  }

  x
}
