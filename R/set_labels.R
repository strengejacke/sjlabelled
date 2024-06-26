#' @title Add value labels to variables
#' @name set_labels
#'
#' @description This function adds labels as attribute (named \code{"labels"})
#'    to a variable or vector \code{x}, resp. to a set of variables in a
#'    data frame or a list-object. A use-case is, for instance, the
#'    \pkg{sjPlot}-package, which supports labelled data and automatically
#'    assigns labels to axes or legends in plots or to be used in tables.
#'    \code{val_labels()} is intended for use within pipe-workflows and has a
#'    tidyverse-consistent syntax, including support for quasi-quotation
#'    (see 'Examples').
#'
#' @seealso See vignette \href{../doc/intro_sjlabelled.html}{Labelled Data and the sjlabelled-Package}
#'            for more details; \code{\link{set_label}} to manually set variable labels or
#'            \code{\link{get_label}} to get variable labels; \code{\link{add_labels}} to
#'            add additional value labels without replacing the existing ones.
#'
#' @param labels (Named) character vector of labels that will be added to \code{x} as
#'          \code{"labels"} or \code{"value.labels"} attribute.
#'          \itemize{
#'            \item if \code{labels} is \strong{not} a \emph{named vector}, its length must equal the value range of \code{x}, i.e. if \code{x} has values from 1 to 3, \code{labels} should have a length of 3;
#'            \item if length of \code{labels} is intended to differ from length of unique values of \code{x}, a warning is given. You can still add missing labels with the \code{force.labels} or \code{force.values} arguments; see 'Note'.
#'            \item if \code{labels} \strong{is} a \emph{named vector}, value labels will be set accordingly, even if \code{x} has a different length of unique values. See 'Note' and 'Examples'.
#'            \item if \code{x} is a data frame, \code{labels} may also be a \code{list} of (named) character vectors;
#'            \item if \code{labels} is a \code{list}, it must have the same length as number of columns of \code{x};
#'            \item if \code{labels} is a vector and \code{x} is a data frame, \code{labels} will be applied to each column of \code{x}.
#'            }
#'          Use \code{labels = ""} to remove labels-attribute from \code{x}.
#' @param force.labels Logical; if \code{TRUE}, all \code{labels} are added as value label
#'          attribute, even if \code{x} has less unique values then length of \code{labels}
#'          or if \code{x} has a smaller range then length of \code{labels}. See 'Examples'.
#'          This parameter will be ignored, if \code{labels} is a named vector.
#' @param force.values Logical, if \code{TRUE} (default) and \code{labels} has less
#'          elements than unique values of \code{x}, additional values not covered
#'          by \code{labels} will be added as label as well. See 'Examples'.
#'          This parameter will be ignored, if \code{labels} is a named vector.
#' @param drop.na Logical, whether existing value labels of tagged NA values
#'          (see \code{\link[haven:tagged_na]{tagged_na}}) should be removed (\code{drop.na = TRUE},
#'          the default) or preserved (\code{drop.na = FALSE}).
#'          See \code{\link{get_na}} for more details on tagged NA values.
#' @param ... For \code{set_labels()}, Optional, unquoted names of variables that should be selected for
#'          further processing. Required, if \code{x} is a data frame (and no
#'          vector) and only selected variables from \code{x} should be processed.
#'          You may also use functions like \code{:} or tidyselect's
#'          select-helpers. \cr \cr For \code{val_labels()},
#'          pairs of named vectors, where the name equals the variable name, which
#'          should be labelled, and the value is the new variable label. \code{val_labels()}
#'          also supports quasi-quotation (see 'Examples').
#'
#' @inheritParams add_labels
#'
#' @return \code{x} with value label attributes; or with removed label-attributes if
#'           \code{labels = ""}. If \code{x} is a data frame, the complete data
#'           frame \code{x} will be returned, with removed or added to variables
#'           specified in \code{...}; if \code{...} is not specified, applies
#'           to all variables in the data frame.
#'
#' @note \itemize{
#'         \item if \code{labels} is a named vector, \code{force.labels} and \code{force.values} will be ignored, and only values defined in \code{labels} will be labelled;
#'         \item if \code{x} has less unique values than \code{labels}, redundant labels will be dropped, see \code{force.labels};
#'         \item if \code{x} has more unique values than \code{labels}, only matching values will be labelled, other values remain unlabelled, see \code{force.values};
#'         }
#'         If you only want to change partial value labels, use \code{\link{add_labels}} instead.
#'         Furthermore, see 'Note' in \code{\link{get_labels}}.
#'
#' @examplesIf require("sjmisc") && require("haven")
#' dummy <- sample(1:4, 40, replace = TRUE)
#' frq(dummy)
#'
#' dummy <- set_labels(dummy, labels = c("very low", "low", "mid", "hi"))
#' frq(dummy)
#'
#' # assign labels with named vector
#' dummy <- sample(1:4, 40, replace = TRUE)
#' dummy <- set_labels(dummy, labels = c("very low" = 1, "very high" = 4))
#' frq(dummy)
#'
#' # force using all labels, even if not all labels
#' # have associated values in vector
#' x <- c(2, 2, 3, 3, 2)
#' # only two value labels
#' x <- set_labels(x, labels = c("1", "2", "3"))
#' x
#' frq(x)
#'
#' # all three value labels
#' x <- set_labels(x, labels = c("1", "2", "3"), force.labels = TRUE)
#' x
#' frq(x)
#'
#' # create vector
#' x <- c(1, 2, 3, 2, 4, NA)
#' # add less labels than values
#' x <- set_labels(x, labels = c("yes", "maybe", "no"), force.values = FALSE)
#' x
#' # add all necessary labels
#' x <- set_labels(x, labels = c("yes", "maybe", "no"), force.values = TRUE)
#' x
#'
#' # set labels and missings
#' x <- c(1, 1, 1, 2, 2, -2, 3, 3, 3, 3, 3, 9)
#' x <- set_labels(x, labels = c("Refused", "One", "Two", "Three", "Missing"))
#' x
#' set_na(x, na = c(-2, 9))
#'
#'
#' x <- labelled(
#'   c(1:3, tagged_na("a", "c", "z"), 4:1),
#'   c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
#'     "Refused" = tagged_na("a"), "Not home" = tagged_na("z"))
#' )
#' # get current NA values
#' x
#' get_na(x)
#' # lose value labels from tagged NA by default, if not specified
#' set_labels(x, labels = c("New Three" = 3))
#' # do not drop na
#' set_labels(x, labels = c("New Three" = 3), drop.na = FALSE)
#'
#'
#' # set labels via named vector,
#' # not using all possible values
#' data(efc)
#' get_labels(efc$e42dep)
#'
#' x <- set_labels(
#'   efc$e42dep,
#'   labels = c(`independent` = 1,
#'              `severe dependency` = 2,
#'              `missing value` = 9)
#'   )
#' get_labels(x, values = "p")
#' get_labels(x, values = "p", non.labelled = TRUE)
#'
#' # labels can also be set for tagged NA value
#' # create numeric vector
#' x <- c(1, 2, 3, 4)
#' # set 2 and 3 as missing, which will automatically set as
#' # tagged NA by 'set_na()'
#' x <- set_na(x, na = c(2, 3))
#' x
#' # set label via named vector just for tagged NA(3)
#' set_labels(x, labels = c(`New Value` = tagged_na("3")))
#'
#' # setting same value labels to multiple vectors
#' dummies <- data.frame(
#'   dummy1 = sample(1:4, 40, replace = TRUE),
#'   dummy2 = sample(1:4, 40, replace = TRUE),
#'   dummy3 = sample(1:4, 40, replace = TRUE)
#' )
#'
#' # and set same value labels for two of three variables
#' test <- set_labels(
#'   dummies, dummy1, dummy2,
#'   labels = c("very low", "low", "mid", "hi")
#' )
#' # see result...
#' get_labels(test)
#'
#' # using quasi-quotation
#' if (require("rlang") && require("dplyr")) {
#'   dummies <- data.frame(
#'     dummy1 = sample(1:4, 40, replace = TRUE),
#'     dummy2 = sample(1:4, 40, replace = TRUE),
#'     dummy3 = sample(1:4, 40, replace = TRUE)
#'   )
#'
#'   x1 <- "dummy1"
#'   x2 <- c("so low", "rather low", "mid", "very hi")
#'
#'   dummies %>%
#'     val_labels(
#'       !!x1 := c("really low", "low", "a bit mid", "hi"),
#'       dummy3 = !!x2
#'     ) %>%
#'     get_labels()
#'
#'   # ... and named vectors to explicitly set value labels
#'   x2 <- c("so low" = 4, "rather low" = 3, "mid" = 2, "very hi" = 1)
#'   dummies %>%
#'     val_labels(
#'       !!x1 := c("really low" = 1, "low" = 3, "a bit mid" = 2, "hi" = 4),
#'       dummy3 = !!x2
#'     ) %>% get_labels(values = "p")
#' }
#' @export
set_labels <- function(x, ...,
                       labels,
                       force.labels = FALSE,
                       force.values = TRUE,
                       drop.na = TRUE) {

  dots <- as.character(match.call(expand.dots = FALSE)$`...`)
  .dat <- .get_dot_data(x, dots)

  # special handling for data frames
  if (is.data.frame(x)) {
    # check if we have one label per variable
    if (is.list(labels) && length(labels) == ncol(.dat)) {
      # get column names
      cn <- colnames(.dat)
      # iterate all columns by number
      for (i in seq_len(ncol(.dat))) {
        x[[cn[i]]] <- set_labels_helper(
          x = .dat[[cn[i]]],
          labels = labels[[i]],
          force.labels = force.labels,
          force.values = force.values,
          drop.na = drop.na,
          var.name = cn[i]
        )
      }
    } else {
      # iterate variables of data frame
      for (i in colnames(.dat)) {
        x[[i]] <- set_labels_helper(
          x = .dat[[i]],
          labels = labels,
          force.labels = force.labels,
          force.values = force.values,
          drop.na = drop.na,
          var.name = i
        )
      }
    }
  } else {
    x <- set_labels_helper(
      x = .dat,
      labels = labels,
      force.labels = force.labels,
      force.values = force.values,
      drop.na = drop.na,
      var.name = NULL
    )
  }

  x
}


#' @importFrom stats na.omit
set_labels_helper <- function(x, labels, force.labels, force.values, drop.na, var.name) {
  # any valid labels? if not, return vector
  if (is.null(labels) || length(labels) == 0) return(x)

  # valid vector?
  if (is.null(x)) {
    warning("Can't add value labels to NULL vectors", call. = FALSE)
    return(x)
  }

  # get labelled / tagged NAs, maybe for later use
  current.na <- get_na(x)

  # check for null
  if (!is.null(labels)) {
    # if labels is empty string, remove labels attribute
    if (length(labels) == 1 && nchar(labels, keepNA = FALSE) == 0) {
      attr(x, "labels") <- NULL

      # set labels for character vectors here!
    } else if (is.character(x)) {
      # string vectors can only get labels of type string
      if (typeof(labels) == typeof(x)) {
        # reverse names and labels
        dummy.labels <- names(labels)
        # but first check if we have named vector or not...
        if (is.null(dummy.labels)) {
          warning("`labels` must be a named vector.", call. = TRUE)
        } else {
          # names(dummy.labels) <- unname(labels)
          # attr(x, "labels") <- dummy.labels
          attr(x, "labels") <- labels
        }
      } else {
        warning("Character vectors can only get labels of same type.", call. = TRUE)
      }

      # set labels for numeric vectors or factors here
    } else {
      # determine value range
      vr <- get_value_range(x)
      # copy values to variables
      valrange <- vr$valrange
      minval <- vr$minval
      maxval <- vr$maxval

      # check for unlisting
      if (is.list(labels)) labels <- labels[[1]]

      # determine amount of labels and unique values
      lablen <- length(labels)
      values <- unique(stats::na.omit(as.vector(x)))
      if (.is_num_chr(values) || .is_num_fac(values)) {
        values <- as.numeric(values)
      }
      values <- sort(values)

      # set var name string
      if (isempty(var.name)) {
        name.string <- "x"
      } else {
        name.string <- var.name
      }

      # check for valid bounds of values
      if (is.infinite(valrange)) {
        warning(sprintf("Can't set value labels for \"%s\". Infinite value range.", name.string), call. = FALSE)

        # check if we have named vector. in this
        # case, just add these values
      } else if (!is.null(names(labels))) {
        if (!requireNamespace("haven", quietly = TRUE)) {
          stop("Package 'haven' required for this function. Please install it.")
        }
        # check names and value attributes. value labels
        # and values might be reversed
        if (!anyNA(suppressWarnings(as.numeric(names(labels)))) &&
            anyNA(suppressWarnings(as.numeric(labels))) &&
            !anyNA(suppressWarnings(as.numeric(values))) &&
            !all(haven::is_tagged_na(labels))) {
          dummy.lab.values <- as.numeric(names((labels)))
          dummy.lab.labels <- as.character(labels)
          labels <- dummy.lab.values
          names(labels) <- dummy.lab.labels
        }

        # sort labels
        labels <- labels[order(labels)]
        # set attributes
        if (anyNA(suppressWarnings(as.numeric(labels)))) {
          # here we have also non-numeric labels, so we set
          # names as character string
          attr(x, "labels") <- labels
        } else {
          # we have only numeric labels, so we set them
          # as numeric values
          attr(x, "labels") <- as.numeric(labels)
        }
        names(attr(x, "labels")) <- as.character(names(labels))
        # check for valid length of labels
        # if amount of labels and values are equal,
        # we assume matching labels
      } else if (length(values) == lablen) {
        # set attributes
        # check whether values is numeric, or - if character -
        # only has numeric character values. If yes, add values
        # as numeric labels-attribute
        if (is.numeric(values) || !anyNA(suppressWarnings(as.numeric(values))))
          attr(x, "labels") <- as.numeric(values)
        else
          attr(x, "labels") <- as.character(values)

        # do we have an ordered factor?
        if (is.ordered(x)) labels <- labels[order(levels(x))]

        names(attr(x, "labels")) <- labels
        # check for valid length of labels
        # here, we have a smaller value range (i.e. less values)
        # than amount of labels
      } else if (valrange < lablen) {
        # do we want to force to set labels, even if we have more labels
        # than values in variable?
        if (force.labels) {
          attr(x, "labels") <- as.numeric(seq_len(lablen))
          names(attr(x, "labels")) <- labels
        } else {
          # we have more labels than values, so just take as many
          # labes as values are present
          message(sprintf("More labels than values of \"%s\". Using first %i labels.", name.string, valrange))
          attr(x, "labels") <- as.numeric(minval:maxval)
          names(attr(x, "labels")) <- labels[seq_len(valrange)]
        }
        # value range is larger than amount of labels. we may
        # have not continuous value range, e.g. "-2" as filter and
        # 1 to 4 as valid values, i.e. -1 and 0 are missing
      } else if (valrange > lablen) {
        # check if user wants to add missing values
        if (force.values) {
          # get amount of unique values
          valrange <- length(values)

          # still no match?
          if (valrange != lablen) {
            # check which one is longer, and get missing values
            add_values <- ifelse(valrange > lablen, valrange[-lablen], lablen[-valrange])
            # add missing values to labels
            labels <- c(labels, as.character(add_values))
            # tell user about modification
            message(sprintf("More values in \"%s\" than length of \"labels\". Additional values were added to labels.", name.string))
          }

          # set attributes
          attr(x, "labels") <- as.numeric(seq_len(valrange))
          names(attr(x, "labels")) <- labels
        } else {
          # tell user about modification
          message(sprintf("\"%s\" has more values than \"labels\", hence not all values are labelled.", name.string))
          # drop values with no associated labels
          attr(x, "labels") <- as.numeric(seq_len(length(labels)))
          names(attr(x, "labels")) <- labels
        }
      } else {
        attr(x, "labels") <- as.numeric(minval:maxval)
        names(attr(x, "labels")) <- labels
      }
    }
    # keep NA's?
    if (!drop.na && !is.null(current.na) && length(current.na) > 0)
      attr(x, "labels") <- c(attr(x, "labels", exact = TRUE), current.na)
  }

  x
}


#' @importFrom stats na.omit
get_value_range <- function(x) {
  # check if var is a factor
  if (is.factor(x)) {
    # check if we have numeric levels
    if (!is.num.fac(x)) {
      # retrieve levels. since levels are numeric, we
      # have minimum and maximum values
      minval <- 1
      maxval <- nlevels(x)
    } else {
      # levels are not numeric. we need to convert them
      # first to retrieve minimum level, as numeric
      minval <- min(as.numeric(levels(x)), na.rm = TRUE)

      # check range, add minimum, so we have max
      maxval <- diff(range(as.numeric(levels(x)))) + minval
    }
  } else if (is.character(x)) {
    # if we have a character vector, we don't have
    # min and max values. instead, we count the
    # amount of unique string values
    minval <- 1
    maxval <- length(unique(stats::na.omit(x)))
  } else if (all(is.na(x))) {
      minval <- 0
      maxval <- 0
  }else {
    # retrieve values
    minval <- as.numeric(min(x, na.rm = TRUE))
    maxval <- as.numeric(max(x, na.rm = TRUE))
  }

  # determine value range
  if (all(is.na(x))) {
    valrange <- 0
  } else{
    valrange <- maxval - minval + 1
  }

  # return all
  list(
    minval = minval,
    maxval = maxval,
    valrange = valrange
  )
}
