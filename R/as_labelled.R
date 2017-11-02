#' @title Convert vector to labelled class
#' @name as_labelled
#'
#' @description Converts a (labelled) vector of any class into a \code{labelled}
#'                class vector, resp. adds a \code{labelled} class-attribute.
#'
#' @param x Variable (vector), \code{data.frame} or \code{list} of variables
#'          that should be converted to \code{\link[haven]{labelled}}-class
#'          objects.
#' @param add.labels Logical, if \code{TRUE}, non-labelled values will be
#'          labelled with the corresponding value.
#' @param add.class Logical, if \code{TRUE}, \code{x} preserves its former
#'          \code{class}-attribute and \code{labelled} is added as additional
#'          attribute. If \code{FALSE} (default), all former \code{class}-attributes
#'          will be removed and the class-attribute of \code{x} will only
#'          be \code{labelled}.
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
#'
#' @importFrom stats na.omit
#' @export
as_labelled <- function(x, add.labels = FALSE, add.class = FALSE) {
  UseMethod("as_labelled")
}

#' @export
as_labelled.data.frame <- function(x, add.labels = FALSE, add.class = FALSE) {
  data.frame(lapply(x, FUN = as_labelled_helper, add.labels, add.class))
}

#' @export
as_labelled.list <- function(x, add.labels = FALSE, add.class = FALSE) {
  lapply(x, FUN = as_labelled_helper, add.labels, add.class)
}

#' @export
as_labelled.default <- function(x, add.labels = FALSE, add.class = FALSE) {
  as_labelled_helper(x, add.labels, add.class)
}

as_labelled_helper <- function(x, add.labels, add.class) {
  # do nothing for labelled class
  if (is_labelled(x)) return(x)

  # if factor, convert to numeric
  if (is.factor(x)) x <- as_numeric(x, keep.labels = T)

  # return atomics
  if (is.null(get_labels(x, attr.only = T))) return(x)

  # fill up missing attributes
  if (add.labels) x <- fill_labels(x)

  # reset missings
  xna <- get_na(x)
  if (!isempty(xna)) x <- set.na(x, na = xna)

  # get former class attributes
  xc <- class(x)
  if (add.class)
    class(x) <- c(xc, "labelled")
  else
    class(x) <- "labelled"

  x
}
