#' @title Convert vector to labelled class
#' @name as_labelled
#'
#' @description Converts a (labelled) vector of any class into a \code{labelled}
#'                class vector, resp. adds a \code{labelled} class-attribute.
#'
#' @param x Variable (vector), \code{data.frame} or \code{list} of variables
#'          that should be converted to \code{\link[haven:labelled]{labelled()}}-class
#'          objects.
#' @param add.labels Logical, if \code{TRUE}, non-labelled values will be
#'          labelled with the corresponding value.
#' @param add.class Logical, if \code{TRUE}, \code{x} preserves its former
#'          \code{class}-attribute and \code{labelled} is added as additional
#'          attribute. If \code{FALSE} (default), all former \code{class}-attributes
#'          will be removed and the class-attribute of \code{x} will only
#'          be \code{labelled}.
#' @param skip.strings Logical, if \code{TRUE}, character vector are not converted
#'   into labelled-vectors. Else, character vectors are converted to factors
#'   vector and the associated values are used as value labels.
#' @param tag.na Logical, if \code{TRUE}, tagged \code{NA} values are replaced
#'   by their associated values. This is required, for instance, when writing
#'   data back to SPSS.
#'
#' @return \code{x}, as \code{labelled}-class object.
#'
#' @examples
#' data(efc)
#' str(efc$e42dep)
#'
#' x <- as_labelled(efc$e42dep)
#' str(x)
#'
#' x <- as_labelled(efc$e42dep, add.class = TRUE)
#' str(x)
#'
#' a <- c(1, 2, 4)
#' x <- as_labelled(a, add.class = TRUE)
#' str(x)
#'
#' data(efc)
#' x <- set_labels(efc$e42dep,
#'                 labels = c(`1` = "independent", `4` = "severe dependency"))
#' x1 <- as_labelled(x, add.labels = FALSE)
#' x2 <- as_labelled(x, add.labels = TRUE)
#'
#' str(x1)
#' str(x2)
#'
#' get_values(x1)
#' get_values(x2)
#' @importFrom stats na.omit
#' @export
as_labelled <- function(x, add.labels = FALSE, add.class = FALSE, skip.strings = FALSE, tag.na = FALSE) {
  UseMethod("as_labelled")
}

#' @export
as_labelled.data.frame <- function(x, add.labels = FALSE, add.class = FALSE, skip.strings = FALSE, tag.na = FALSE) {
  data_frame(lapply(x, FUN = as_labelled_helper, add.labels, add.class, skip.strings, tag.na))
}

#' @export
as_labelled.list <- function(x, add.labels = FALSE, add.class = FALSE, skip.strings = FALSE, tag.na = FALSE) {
  lapply(x, FUN = as_labelled_helper, add.labels, add.class, skip.strings, tag.na)
}

#' @export
as_labelled.default <- function(x, add.labels = FALSE, add.class = FALSE, skip.strings = FALSE, tag.na = FALSE) {
  as_labelled_helper(x, add.labels, add.class, skip.strings, tag.na)
}

as_labelled_helper <- function(x, add.labels, add.class, skip.strings, tag.na) {
  # do nothing for labelled class
  if (is_labelled(x)) return(x)

  if (is.character(x) && skip.strings) return(x)

  # if factor, convert to numeric
  if (is.factor(x)) x <- as_numeric(x, keep.labels = TRUE)

  # return atomics
  if (is.null(get_labels(x, attr.only = TRUE))) return(x)

  # fill up missing attributes
  if (add.labels) x <- fill_labels(x)

  # reset missings
  if (!tag.na) {
    xna <- get_na(x)
    if (!isempty(xna)) {
      x <- set_na(x, na = xna)
    }
  } else {
    if (!requireNamespace("haven", quietly = TRUE)) {
      stop("Package 'haven' required for this function. Please install it.")
    }
    labels <- attr(x, "labels", exact = TRUE)
    xna <- get_na(x, as.tag = TRUE)
    new_tags <- unname(gsub("NA\\((.*)\\)", "\\1", xna))
    names(new_tags) <- new_tags
    # convert to numeric, if character
    numeric_na <- which(is.na(suppressWarnings(as.numeric(new_tags))))
    if (any(numeric_na)) {
      names(new_tags)[numeric_na] <- match(new_tags[numeric_na], letters) * -1
    }
    tagged_missing <- haven::na_tag(x)
    for (i in 1:length(xna)) {
      x[which(tagged_missing == new_tags[i])] <- as.numeric(names(new_tags[i]))
    }
    labels[is.na(labels)] <- stats::setNames(attr(x, "na.values"), names(labels[is.na(labels)]))
    attr(x, "labels") <- labels
  }

  # is type of labels same as type of vector? typically, character
  # vectors can have numeric labels or vice versa, numeric vectors
  # have "numeric" labels as character strings. in this case,
  # harmonize types of vector and labels, so haven doesn't complain

  lt <- as.vector(attr(x, "labels", exact = TRUE))
  if (!is.null(lt) && typeof(lt) != typeof(x)) {
    lab.at <- attr(x, "labels", exact = TRUE)
    nlab <- names(lab.at)
    if (is.num.chr(lt, na.rm = TRUE)) {
      lab.at <- as.numeric(lab.at)
      names(lab.at) <- nlab
    } else {
      lab.at <- as.character(lab.at)
      names(lab.at) <- nlab
    }
    attr(x, "labels") <- lab.at
  }

  # get former class attributes
  xc <- class(x)
  if (add.class)
    class(x) <- c(xc, "haven_labelled", "vctrs_vctr")
  else
    class(x) <- c("haven_labelled", "vctrs_vctr")

  x
}
