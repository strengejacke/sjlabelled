#' @title Retrieve variable label(s) of labelled data
#' @name get_label
#'
#' @description This function returns the variable labels of labelled data.
#'
#' @seealso See vignette \href{../doc/intro_sjlabelled.html}{Labelled Data and the sjlabelled-Package}
#'            for more details; \code{\link{set_label}} to manually set variable labels or \code{\link{get_labels}}
#'            to get value labels; \code{\link{var_labels}} to set multiple variable
#'            labels at once.

#' @param x A data frame with variables that have label attributes (e.g.
#'          from an imported SPSS, SAS or STATA data set, via \code{\link{read_spss}},
#'          \code{\link{read_sas}} or \code{\link{read_stata}}); a variable
#'          (vector) with variable label attribute; or a \code{list} of variables
#'          with variable label attributes. See 'Examples'.
#' @param ... Optional, names of variables, where labels should be retrieved.
#'            Required, if either data is a data frame and no vector, or if only
#'            selected variables from \code{x} should be used in the function.
#'            Convenient argument to work with pipe-chains (see 'Examples').
#' @param def.value Optional, a character string which will be returned as label
#'          if \code{x} has no label attribute. By default, \code{NULL} is returned.
#'
#' @inheritParams get_term_labels
#'
#' @return A named character vector with all variable labels from the data frame or list;
#'           or a simple character vector (of length 1) with the variable label, if \code{x} is a variable.
#'           If \code{x} is a single vector and has no label attribute, the value
#'           of \code{def.value} will be returned (which is by default \code{NULL}).
#'
#' @note \code{\link{var_labels}} is an alternative way to set variable labels,
#'       which follows the philosophy of tidyvers API design (data as first argument,
#'       dots as value pairs indicating variables)
#'
#' @examples
#' # import SPSS data set
#' # mydat <- read_spss("my_spss_data.sav", enc="UTF-8")
#'
#' # retrieve variable labels
#' # mydat.var <- get_label(mydat)
#'
#' # retrieve value labels
#' # mydat.val <- get_labels(mydat)
#'
#' data(efc)
#'
#' # get variable lable
#' get_label(efc$e42dep)
#'
#' # alternative way
#' get_label(efc)["e42dep"]
#'
#' # 'get_label()' also works within pipe-chains
#' efc %>% get_label(e42dep, e16sex)
#'
#' # set default values
#' get_label(mtcars, mpg, cyl, def.value = "no var labels")
#'
#' # simple barplot
#' barplot(table(efc$e42dep))
#' # get value labels to annotate barplot
#' barplot(table(efc$e42dep),
#'         names.arg = get_labels(efc$e42dep),
#'         main = get_label(efc$e42dep))
#'
#' # get labels from multiple variables
#' get_label(list(efc$e42dep, efc$e16sex, efc$e15relat))
#'
#' # use case conversion for human-readable labels
#' data(iris)
#' get_label(iris, def.value = colnames(iris))
#' get_label(iris, def.value = colnames(iris), case = "parsed")
#' @export
get_label <- function(x, ..., def.value = NULL, case = NULL) {
  UseMethod("get_label")
}


#' @export
get_label.data.frame <- function(x, ..., def.value = NULL, case = NULL) {
  dots <- as.character(match.call(expand.dots = FALSE)$`...`)
  x <- .get_dot_data(x, dots)

  sapply(seq_along(x), function(i) {
    # get label
    label <- attr(x[[i]], "label", exact = T)

    # any label?
    if (is.null(label)) {
      if (!is.null(def.value)) {
        # def.value may also apply to data frame arguments,
        # so it can be greater than length one
        if (i <= length(def.value))
          label <- def.value[i]
        else
          label <- def.value
      } else {
        label <- ""
      }
    }

    names(label) <- colnames(x)[i]
    # append to return result
    convert_case(label, case)
  })
}


#' @export
get_label.list <- function(x, ..., def.value = NULL, case = NULL) {
  convert_case(unlist(lapply(x, attr, "label", exact = T)), case)
}


#' @export
get_label.default <- function(x, ..., def.value = NULL, case = NULL) {
  labels <- attr(x, "label", exact = T)

  if (is.null(labels))
    convert_case(def.value, case)
  else
    convert_case(labels, case)
}
