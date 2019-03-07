#' @rdname zap_labels
#' @export
fill_labels <- function(x, ...) {
  # evaluate arguments, generate data
  .dat <- get_dot_data(x, rlang::quos(...))

  if (is.data.frame(x)) {
    # iterate variables of data frame
    for (i in colnames(.dat)) {
      x[[i]] <- fill_labels_helper(.dat[[i]])
    }
  } else {
    x <- fill_labels_helper(.dat)
  }

  x
}

fill_labels_helper <- function(x) {
  # get current labels
  current.values <- get_labels(x, attr.only = T, non.labelled = F)
  # get all labels, including non-labelled values
  all.values <- get_labels(x,
                           attr.only = T,
                           values = "n",
                           non.labelled = T)
  # have any values?
  if (!is.null(all.values)) {
    # set back all labels, if amount of all labels differ
    # from the "current" values
    if (length(all.values) > length(current.values)) {
      # first, we need to switch name attribute and values
      all.val.switch <- as.numeric(names(all.values))
      names(all.val.switch) <- as.character(all.values)
      # get current NA values
      current.na <- get_na(x)
      # add NA
      if (!is.null(current.na)) all.val.switch <- c(all.val.switch, current.na)
      # then set labels
      x <- set_labels(
        x,
        labels = all.val.switch,
        force.labels = T,
        force.values = T
      )
    }
  }

  x
}
