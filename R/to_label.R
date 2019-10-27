#' @title Convert variable into factor with associated value labels
#' @name as_label
#'
#' @description \code{as_label()} converts (replaces) values of a variable (also of factors
#'    or character vectors) with their associated value labels. Might
#'    be helpful for factor variables.
#'    For instance, if you have a Gender variable with 0/1 value, and associated
#'    labels are male/female, this function would convert all 0 to male and
#'    all 1 to female and returns the new variable as factor.
#'    \code{as_character()} does the same as \code{as_label()}, but returns
#'    a character vector.
#'
#' @param add.non.labelled Logical, if \code{TRUE}, values without associated
#'    value label will also be converted to labels (as is). See 'Examples'.
#' @param prefix Logical, if \code{TRUE}, the value labels used as factor levels
#'    or character values will be prefixed with their associated values. See 'Examples'.
#' @param var.label Optional string, to set variable label attribute for the
#'    returned variable (see vignette \href{../doc/intro_sjlabelled.html}{Labelled Data and the sjlabelled-Package}).
#'    If \code{NULL} (default), variable label attribute of \code{x} will
#'    be used (if present). If empty, variable label attributes will be removed.
#' @param drop.na Logical, if \code{TRUE}, tagged \code{NA} values with value labels
#'    will be converted to regular NA's. Else, tagged \code{NA} values will be replaced
#'    with their value labels. See 'Examples' and \code{\link{get_na}}.
#' @param drop.levels Logical, if \code{TRUE}, unused factor levels will be
#'    dropped (i.e. \code{\link{droplevels}} will be applied before returning
#'    the result).
#' @param keep.labels Logical, if \code{TRUE}, value labels are preserved This
#'    allows users to quickly convert back factors to numeric vectors with
#'    \code{as_numeric()}.
#'
#' @inheritParams add_labels
#'
#' @return A factor with the associated value labels as factor levels. If \code{x}
#'           is a data frame, the complete data frame \code{x} will be returned,
#'           where variables specified in \code{...} are coerced to factors;
#'           if \code{...} is not specified, applies to all variables in the
#'           data frame. \code{as_character()} returns a character vector.
#'
#' @note Value label attributes (see \code{\link{get_labels}})
#'       will be removed when converting variables to factors.
#'
#' @details See 'Details' in \code{\link{get_na}}.
#'
#' @examples
#' data(efc)
#' print(get_labels(efc)['c161sex'])
#' head(efc$c161sex)
#' head(as_label(efc$c161sex))
#'
#' print(get_labels(efc)['e42dep'])
#' table(efc$e42dep)
#' table(as_label(efc$e42dep))
#'
#' head(efc$e42dep)
#' head(as_label(efc$e42dep))
#'
#' # structure of numeric values won't be changed
#' # by this function, it only applies to labelled vectors
#' # (typically categorical or factor variables)
#'
#' str(efc$e17age)
#' str(as_label(efc$e17age))
#'
#'
#' # factor with non-numeric levels
#' as_label(factor(c("a", "b", "c")))
#'
#' # factor with non-numeric levels, prefixed
#' x <- factor(c("a", "b", "c"))
#' x <- set_labels(x, labels = c("ape", "bear", "cat"))
#' as_label(x, prefix = TRUE)
#'
#'
#' # create vector
#' x <- c(1, 2, 3, 2, 4, NA)
#' # add less labels than values
#' x <- set_labels(
#'   x,
#'   labels = c("yes", "maybe", "no"),
#'   force.labels = FALSE,
#'   force.values = FALSE
#' )
#'
#' # convert to label w/o non-labelled values
#' as_label(x)
#'
#' # convert to label, including non-labelled values
#' as_label(x, add.non.labelled = TRUE)
#'
#'
#' # create labelled integer, with missing flag
#' library(haven)
#' x <- labelled(
#'   c(1:3, tagged_na("a", "c", "z"), 4:1, 2:3),
#'   c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
#'     "Refused" = tagged_na("a"), "Not home" = tagged_na("z"))
#' )
#'
#' # to labelled factor, with missing labels
#' as_label(x, drop.na = FALSE)
#'
#' # to labelled factor, missings removed
#' as_label(x, drop.na = TRUE)
#'
#' # keep missings, and use non-labelled values as well
#' as_label(x, add.non.labelled = TRUE, drop.na = FALSE)
#'
#'
#' # convert labelled character to factor
#' dummy <- c("M", "F", "F", "X")
#' dummy <- set_labels(
#'   dummy,
#'   labels = c(`M` = "Male", `F` = "Female", `X` = "Refused")
#' )
#' get_labels(dummy,, "p")
#' as_label(dummy)
#'
#' # drop unused factor levels, but preserve variable label
#' x <- factor(c("a", "b", "c"), levels = c("a", "b", "c", "d"))
#' x <- set_labels(x, labels = c("ape", "bear", "cat"))
#' set_label(x) <- "A factor!"
#' x
#' as_label(x, drop.levels = TRUE)
#'
#' # change variable label
#' as_label(x, var.label = "New variable label!", drop.levels = TRUE)
#'
#'
#' # convert to numeric and back again, preserving label attributes
#' # *and* values in numeric vector
#' x <- c(0, 1, 0, 4)
#' x <- set_labels(x, labels = c(`null` = 0, `one` = 1, `four` = 4))
#'
#' # to factor
#' as_label(x)
#'
#' # to factor, back to numeric - values are 1, 2 and 3,
#' # instead of original 0, 1 and 4
#' as_numeric(as_label(x))
#'
#' # preserve label-attributes when converting to factor, use these attributes
#' # to restore original numeric values when converting back to numeric
#' as_numeric(as_label(x, keep.labels = TRUE), use.labels = TRUE)
#'
#'
#' # easily coerce specific variables in a data frame to factor
#' # and keep other variables, with their class preserved
#' as_label(efc, e42dep, e16sex, c172code)
#' @export
as_label <- function(x, ..., add.non.labelled = FALSE, prefix = FALSE, var.label = NULL, drop.na = TRUE, drop.levels = FALSE, keep.labels = FALSE) {
  UseMethod("as_label")
}


#' @export
as_label.default <- function(x, ..., add.non.labelled = FALSE, prefix = FALSE, var.label = NULL, drop.na = TRUE, drop.levels = FALSE, keep.labels = FALSE) {
  dots <- as.character(match.call(expand.dots = FALSE)$`...`)
  .dat <- .get_dot_data(x, dots)

  as_label_helper(.dat, add.non.labelled, prefix, var.label, drop.na, drop.levels, keep.labels)
}


#' @export
as_label.data.frame <- function(x, ..., add.non.labelled = FALSE, prefix = FALSE, var.label = NULL, drop.na = TRUE, drop.levels = FALSE, keep.labels = FALSE) {
  dots <- as.character(match.call(expand.dots = FALSE)$`...`)
  .dat <- .get_dot_data(x, dots)

  # iterate variables of data frame
  for (i in colnames(.dat)) {
    x[[i]] <- as_label_helper(.dat[[i]], add.non.labelled, prefix, var.label, drop.na, drop.levels, keep.labels)
  }

  x
}


#' @importFrom haven na_tag is_tagged_na
as_label_helper <- function(x, add.non.labelled, prefix, var.label, drop.na, drop.levels, keep.labels) {
  # prefix labels?
  if (prefix)
    iv <- "p"
  else
    iv <- 0

  # retrieve variable label
  if (is.null(var.label))
    var_lab <- get_label(x)
  else
    var_lab <- var.label

  # get labels
  labels <- NULL

  # keep missings?
  if (!drop.na) {
    # get NA
    current.na <- get_na(x)

    # any NA?
    if (!is.null(current.na)) {
      # we have to set all NA labels at once, else NA loses tag
      # so we prepare a dummy label-vector, where we copy all different
      # NA labels to `x` afterwards
      dummy_na <- rep("", times = length(x))

      # iterare NA
      for (i in seq_len(length(current.na))) {
        dummy_na[haven::na_tag(x) == haven::na_tag(current.na[i])] <- names(current.na)[i]
      }

      x[haven::is_tagged_na(x)] <- dummy_na[haven::is_tagged_na(x)]
    }
  } else {
    # in case x has tagged NA's we need to be sure to convert
    # those into regular NA's, because else saving would not work
    x[is.na(x)] <- NA
  }

  # get value labels
  vl <- get_labels(x, attr.only = TRUE, values = iv,
                   non.labelled = add.non.labelled,
                   drop.na = drop.na)

  # check if we have any labels, else
  # return variable "as is"
  if (!is.null(vl)) {
    # get associated values for value labels
    vnn <- labels <- get_labels(
      x,
      attr.only = TRUE,
      values = "n",
      non.labelled = add.non.labelled,
      drop.na = drop.na
    )

    # convert to numeric
    vn <- suppressWarnings(as.numeric(names(vnn)))

    # where some values non-numeric? if yes,
    # use value names as character values
    if (anyNA(vn)) vn <- names(vnn)

    # replace values with labels
    if (is.factor(x)) {
      # more levels than labels?
      remain_labels <- levels(x)[!levels(x) %in% vn]
      # set new levels
      levels(x) <- c(vl, remain_labels)
      # remove attributes
      x <- remove_all_labels(x)
    } else {
      for (i in seq_len(length(vl))) {
        #if label is number, prevents loop from replacing again
        x[x == vn[i]] <- paste0(vl[i], "_X_")
      }
      # remove suffix
      x <- gsub("_X_$", "", x)
      # to factor
      x <- factor(x, levels = unique(vl))
    }
  }

  # drop unused levels?
  if (drop.levels && is.factor(x)) x <- droplevels(x)

  # set back variable labels
  if (!is.null(var_lab)) x <- suppressWarnings(set_label(x, label = var_lab))

  # check if we should set back former variable and value labels
  if (keep.labels && !prefix) {
    labels.names <- names(labels)
    labels.values <- unname(labels)
    labels <- labels.names
    names(labels) <- labels.values
    x <- set_labels(x, labels = labels, force.labels = T)
  }

  # return as factor
  x
}


#' @rdname as_label
#' @export
as_character <- function(x, ..., add.non.labelled = FALSE, prefix = FALSE, var.label = NULL, drop.na = TRUE, drop.levels = FALSE) {
  dots <- as.character(match.call(expand.dots = FALSE)$`...`)
  .dat <- .get_dot_data(x, dots)

  # get variable labels
  vl <- get_label(x)

  if (is.data.frame(x)) {

    # iterate variables of data frame
    for (i in colnames(.dat)) {
      x[[i]] <- as.character(as_label_helper(.dat[[i]], add.non.labelled, prefix, var.label, drop.na, drop.levels, keep.labels = FALSE))
    }
  } else {
    x <- as.character(as_label_helper(.dat, add.non.labelled, prefix, var.label, drop.na, drop.levels, keep.labels = FALSE))
  }

  # set back variable labels, if any
  if (!is.null(vl)) x <- set_label(x, vl)

  x
}
