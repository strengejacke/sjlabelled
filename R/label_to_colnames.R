#' @title Use variable labels as column names
#' @name label_to_colnames
#'
#' @description This function sets variable labels as column names, to use "labelled
#'   data" also for those functions that cannot cope with labelled data by default.
#'
#' @param x A data frame.
#' @inheritParams as_factor
#'
#' @return \code{x} with variable labels as column names. For variables without
#'   variable labels, the column name is left unchanged.
#'
#' @examples
#' data(iris)
#'
#' iris <- var_labels(
#'   iris,
#'   Petal.Length = "Petal length (cm)",
#'   Petal.Width = "Petal width (cm)"
#' )
#'
#' colnames(iris)
#' plot(iris)
#'
#' colnames(label_to_colnames(iris))
#' plot(label_to_colnames(iris))
#' @export
label_to_colnames <- function(x, ...) {
  dots <- as.character(match.call(expand.dots = FALSE)$`...`)
  .dat <- .get_dot_data(x, dots)

  if (!is.null(ncol(.dat)) && ncol(.dat) > 0) {
    replace_index <- match(colnames(.dat), colnames(x))
    colnames(x)[replace_index] <- get_label(.dat, def.value = colnames(.dat))
  } else {
    colnames(x) <- get_label(x, def.value = colnames(x))
  }

  x
}