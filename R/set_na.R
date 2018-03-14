set.na <- function(x, ..., na, drop.levels = TRUE, as.tag = FALSE) {
  # check for valid value
  if (is.null(na) || is.na(na)) return(x)

  # evaluate arguments, generate data
  .dat <- get_dot_data(x, dplyr::quos(...))

  if (is.data.frame(x)) {
    # iterate variables of data frame
    for (i in colnames(.dat)) {
      x[[i]] <- set_na_helper(
        x = .dat[[i]],
        value = na,
        drop.levels = drop.levels,
        as.tag = as.tag
      )
    }
    # coerce to tibble
    x <- tibble::as_tibble(x)
  } else {
    x <- set_na_helper(
      x = .dat,
      value = na,
      drop.levels = drop.levels,
      as.tag = as.tag
    )
  }

  x
}

#' @importFrom stats na.omit
#' @importFrom haven tagged_na na_tag
set_na_helper <- function(x, value, drop.levels, as.tag) {
  # check if values has only NA's
  if (sum(is.na(x)) == length(x)) return(x)

  # check if value is a named vector
  na.names <- names(value)
  # get values for value labels
  lab.values <- get_values(x, drop.na = F)

  # no tagged NA's for date values
  if (inherits(x, "Date")) as.tag <- F

  # get value labels
  val.lab <- attr(x, "labels", exact = T)
  val.lab <- val.lab[!haven::is_tagged_na(val.lab)]

  # if value is a character vector, user may have defined a value label.
  # find value of associated label then
  if (is.character(value)) {
    # get value labels that match the values which should be set to NA
    val.match <- val.lab[val.lab %in% value]
    # now get values for this vector
    if (!isempty(val.match) && !isempty(names(val.match))) {
      # should be numeric, else we might have a factor
      na.values <- suppressWarnings(as.numeric(val.match))
      # if we have no NA, coercing to numeric worked. Now get these
      # NA values and remove value labels from vector
      if (!anyNA(na.values)) {
        x <- suppressWarnings(remove_labels(x, value = value))
        value <- na.values
      }
    }
  }

  # haven::na_tag works only for double
  if (is.double(x) && as.tag) {
    # get na-tags, to check whether NA already was defined
    nat <- as.vector(stats::na.omit(haven::na_tag(x)))
    # stop if user wants to assign a value to NA that is
    # already assigned as NA
    if (any(nat %in% as.character(value)))
      stop("Can't set NA values. At least one element of `value` is already defined as NA. Use `zap_na_tags()` to remove tags from NA values.", call. = F)
  }

  # iterate all NAs
  for (i in seq_len(length(value))) {
    if (as.tag) {
      # find associated values in x and set them as tagged NA
      x[x %in% value[i]] <- haven::tagged_na(as.character(value[i]))
      # is na-value in labelled values?
      lv <- which(lab.values == value[i])
      # if yes, replace label
      if (!isempty(lv)) {
        # for tagged NA, use tag as new attribute
        # change value
        attr(x, "labels")[lv] <- haven::tagged_na(as.character(value[i]))
        # change label as well?
        if (!is.null(na.names)) names(attr(x, "labels"))[lv] <- na.names[i]
      } else {
        # get labels and label values
        lv <- attr(x, "labels", exact = T)
        ln <- names(attr(x, "labels", exact = T))
        # add NA
        attr(x, "labels") <- c(lv, haven::tagged_na(as.character(value[i])))
        if (!is.null(na.names))
          names(attr(x, "labels")) <- c(ln, na.names[i])
        else
          names(attr(x, "labels")) <- c(ln, as.character(value[i]))
      }
    } else {
      # find associated values in x and set them as tagged NA
      x[x %in% value[i]] <- NA
    }
  }

  # remove unused value labels
  removers <- which(get_values(x) %in% value)

  if (!is.null(removers) && !isempty(removers, first.only = T)) {
    attr(x, "labels") <- val.lab[-removers]
  }

  # if we have a factor, check if we have unused levels now due to NA
  # assignment. If yes, drop levels
  if (is.factor(x) && drop.levels && length(levels(x)) != length(levels(droplevels(x)))) {
    # save value and variable labels
    keep.val <- attr(x, "labels", exact = T)
    keep.var <- attr(x, "label", exact = T)

    # drop levels
    x <- droplevels(x)

    # set back labels
    attr(x, "labels") <- keep.val
    attr(x, "label") <- keep.var
  }

  x
}
