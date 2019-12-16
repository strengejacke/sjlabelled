#' @title Retrieve value labels of labelled data
#' @name get_labels
#'
#' @description This function returns the value labels of labelled data.
#'
#' @seealso See vignette \href{../doc/intro_sjlabelled.html}{Labelled Data and the sjlabelled-Package}
#'            for more details; \code{\link{set_labels}} to manually set value
#'            labels, \code{\link{get_label}} to get variable labels and
#'            \code{\link{get_values}} to retrieve the values associated
#'            with value labels.
#'
#' @param x A data frame with variables that have value label attributes (e.g.
#'          from an imported SPSS, SAS or STATA data set, via \code{\link{read_spss}},
#'          \code{\link{read_sas}} or \code{\link{read_stata}}); a variable
#'          (vector) with value label attributes; or a \code{list} of variables
#'          with values label attributes. If \code{x} has no label attributes,
#'          factor levels are returned. See 'Examples'.
#' @param values String, indicating whether the values associated with the
#'          value labels are returned as well. If \code{values = "as.name"}
#'          (or \code{values = "n"}), values are set as \code{names}
#'          attribute of the returned object. If \code{values = "as.prefix"}
#'          (or \code{values = "p"}), values are included as prefix
#'          to each label. See 'Examples'.
#' @param attr.only Logical, if \code{TRUE}, labels are only searched for
#'          in the the vector's \code{attributes}; else, if \code{attr.only = FALSE}
#'          and \code{x} has no label attributes, factor levels or string values
#'          are returned. See 'Examples'.
#' @param non.labelled Logical, if \code{TRUE}, values without labels will
#'          also be included in the returned labels (see \code{\link{fill_labels}}).
#' @param drop.na Logical, whether labels of tagged NA values (see \code{\link[haven]{tagged_na}})
#'          should be included in the return value or not. By default, labelled
#'          (tagged) missing values are not returned. See \code{\link{get_na}}
#'          for more details on tagged NA values.
#' @param drop.unused Logical, if \code{TRUE}, unused labels will be removed from
#'          the return value.
#' @return Either a list with all value labels from all variables if \code{x}
#'           is a \code{data.frame} or \code{list}; a string with the value
#'           labels, if \code{x} is a variable;
#'           or \code{NULL} if no value label attribute was found.
#'
#' @examples
#' # import SPSS data set
#' # mydat <- read_spss("my_spss_data.sav")
#'
#' # retrieve variable labels
#' # mydat.var <- get_label(mydat)
#'
#' # retrieve value labels
#' # mydat.val <- get_labels(mydat)
#'
#' data(efc)
#' get_labels(efc$e42dep)
#'
#' # simple barplot
#' barplot(table(efc$e42dep))
#' # get value labels to annotate barplot
#' barplot(table(efc$e42dep),
#'         names.arg = get_labels(efc$e42dep),
#'         main = get_label(efc$e42dep))
#'
#' # include associated values
#' get_labels(efc$e42dep, values = "as.name")
#'
#' # include associated values
#' get_labels(efc$e42dep, values = "as.prefix")
#'
#' # get labels from multiple variables
#' get_labels(list(efc$e42dep, efc$e16sex, efc$e15relat))
#'
#'
#' # create a dummy factor
#' f1 <- factor(c("hi", "low", "mid"))
#' # search for label attributes only
#' get_labels(f1, attr.only = TRUE)
#' # search for factor levels as well
#' get_labels(f1)
#'
#' # same for character vectors
#' c1 <- c("higher", "lower", "mid")
#' # search for label attributes only
#' get_labels(c1, attr.only = TRUE)
#' # search for string values as well
#' get_labels(c1)
#'
#'
#' # create vector
#' x <- c(1, 2, 3, 2, 4, NA)
#' # add less labels than values
#' x <- set_labels(x, labels = c("yes", "maybe", "no"), force.values = FALSE)
#' # get labels for labelled values only
#' get_labels(x)
#' # get labels for all values
#' get_labels(x, non.labelled = TRUE)
#'
#'
#' # get labels, including tagged NA values
#' library(haven)
#' x <- labelled(c(1:3, tagged_na("a", "c", "z"), 4:1),
#'               c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
#'                 "Refused" = tagged_na("a"), "Not home" = tagged_na("z")))
#' # get current NA values
#' x
#' get_labels(x, values = "n", drop.na = FALSE)
#'
#'
#' # create vector with unused labels
#' data(efc)
#' efc$e42dep <- set_labels(
#'   efc$e42dep,
#'   labels = c("independent" = 1, "dependent" = 4, "not used" = 5)
#' )
#' get_labels(efc$e42dep)
#' get_labels(efc$e42dep, drop.unused = TRUE)
#' get_labels(efc$e42dep, non.labelled = TRUE, drop.unused = TRUE)
#' @export
get_labels <- function(x, attr.only = FALSE, values = NULL,
                       non.labelled = FALSE, drop.na = TRUE, drop.unused = FALSE) {
  UseMethod("get_labels")
}

#' @export
get_labels.data.frame <- function(x, attr.only = FALSE, values = NULL,
                                  non.labelled = FALSE, drop.na = TRUE,
                                  drop.unused = FALSE) {

  lapply(x, FUN = get_labels_helper, attr.only = attr.only, include.values = values,
         include.non.labelled = non.labelled, drop.na = drop.na, drop.unused = drop.unused)
}

#' @export
get_labels.list <- function(x, attr.only = FALSE, values = NULL,
                            non.labelled = FALSE, drop.na = TRUE,
                            drop.unused = FALSE) {

  lapply(x, FUN = get_labels_helper, attr.only = attr.only, include.values = values,
         include.non.labelled = non.labelled, drop.na = drop.na, drop.unused = drop.unused)
}

#' @export
get_labels.default <- function(x, attr.only = FALSE, values = NULL,
                               non.labelled = FALSE, drop.na = TRUE,
                               drop.unused = FALSE) {

  get_labels_helper(x, attr.only = attr.only, include.values = values,
                    include.non.labelled = non.labelled, drop.na = drop.na,
                    drop.unused = drop.unused)
}

# Retrieve value labels of a data frame or variable
# See 'get_labels'
get_labels_helper <- function(x, attr.only, include.values, include.non.labelled, drop.na, drop.unused) {

  labels <- attr(x, "labels", exact = TRUE)
  add_vals <- NULL

  # if variable has no label attribute, use factor levels as labels
  if (is.null(labels)) {
    # only use factor level if explicitly chosen by user
    if (!attr.only) {
      # get levels of vector
      lv <- levels(x)
      # do we have any levels?
      if (!is.null(lv)) {
        labels <- lv
      } else if (is.character(x)) {
        # finally, if we even don't have values, check for
        # character elements
        labels <- unique(x)
      }
    }
  } else {
    if (!requireNamespace("haven", quietly = TRUE)) {
      stop("Package 'haven' required for this function. Please install it.")
    }
    # drop na?
    if (drop.na) labels <- labels[!haven::is_tagged_na(labels)]

    # check if we have anything
    if (!is.null(labels) && length(labels) > 0) {
      # sort labels
      labels <- labels[order(labels)]

      # retrieve values associated with labels. for character vectors
      # or factors with character levels, these values are character values,
      # else, they are numeric values
      if (is.character(x) || (is.factor(x) && !is.num.fac(x)))
        values <- unname(labels)
      else
        values <- as.numeric(unname(labels))

      # retrieve label values in correct order
      labels <- names(labels)

      # do we have any tagged NAs? If so, get tagged NAs
      # and annotate them properly
      if (any(haven::is_tagged_na(values))) {
        values[haven::is_tagged_na(values)] <-
          paste0("NA(", haven::na_tag(values[haven::is_tagged_na(values)]), ")")
      }

      # do we want to include non-labelled values as well? if yes,
      # find all values in variable that have no label attributes
      if (include.non.labelled) {
        # get values of variable
        valid.vals <- sort(unique(stats::na.omit(as.vector(x))))

        # check if we have different amount values than labels
        # or, if we have same amount of values and labels, whether
        # values and labels match or not
        if (length(valid.vals) != length(labels) || anyNA(match(values, valid.vals))) {
          # We now need to know, which values of "x" don't
          # have labels.
          add_vals <- valid.vals[!valid.vals %in% values]
          # add to labels
          labels <- c(labels, as.character(add_vals))
          # fix value prefix
          new_vals <- c(as.character(values), as.character(add_vals))

          # check if values are numeric or not. if not,
          # make sure it's character, so we can order
          # consistently
          if (suppressWarnings(anyNA(as.numeric(values))))
            orderpart <- as.character(values)
          else
            orderpart <- as.numeric(values)

          # sort values and labels
          labels <- labels[order(c(orderpart, add_vals))]
          new_vals <- new_vals[order(c(orderpart, add_vals))]

          # set back new values
          values <- new_vals
        }
      }

      # include associated values?
      if (!is.null(include.values)) {
        # for backwards compatibility, we also accept "TRUE"
        # here we set values as names-attribute
        if ((is.logical(include.values) && isTRUE(include.values)) ||
            include.values == "as.name" || include.values == "n") {
          names(labels) <- values
        }

        # here we include values as prefix of labels
        if (include.values == "as.prefix" || include.values == "p") {
          if (is.numeric(values))
            labels <- sprintf("[%i] %s", values, labels)
          else
            labels <- sprintf("[%s] %s", values, labels)
        }
      }
    }
  }

  # drop unused labels with no values in data
  if (drop.unused) {
    # get all values
    av <- c(get_values(x, drop.na = drop.na), add_vals)
    # drop unused values
    if (!is.null(av)) labels <- labels[sort(av) %in% names(table(x))]
  }

  # return them
  labels
}
