#' @title Generic case conversion for labels
#' @name convert_case
#'
#' @description This function wraps \code{to_any_case()} from the \pkg{snakecase}
#'   package with certain defaults for the \code{sep_in} and
#'   \code{sep_out} arguments, used for instance to convert cases in
#'   \code{\link{term_labels}}.
#'
#' @param lab Character vector that should be case converted.
#' @param case Desired target case. Labels will automatically converted into the
#'          specified character case. See \code{\link[snakecase]{to_any_case}} for
#'          more details on this argument.
#' @param verbose Toggle warnings and messages on or off.
#' @param ... Further arguments passed down to \code{\link[snakecase]{to_any_case}},
#'        like \code{sep_in} or \code{sep_out}.
#'
#' @return \code{lab}, with converted case.
#'
#' @details When calling \code{to_any_case()} from \pkg{snakecase}, the
#'   \code{sep_in} argument is set to \code{"(?<!\\\\d)\\\\."}, and the
#'   \code{sep_out} to \code{" "}. This gives feasible results from variable
#'   labels for plot annotations.
#'
#' @examples
#' data(iris)
#' convert_case(colnames(iris))
#' convert_case(colnames(iris), case = "snake")
#' @export
convert_case <- function(lab, case = NULL, verbose = FALSE, ...) {

  if (!requireNamespace("snakecase", quietly = TRUE)) {
    if (verbose)
      message("Package `snakecase` needs to be installed for case-conversion.")
    return(lab)
  }

  if (!is.null(case) && !is.null(lab)) {

    # set defaults

    prep <- "(?<!\\d)\\."
    posp <- " "


    # check additional arguments

    add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

    if ("preprocess" %in% names(add.args)) prep <- eval(add.args[["preprocess"]])
    if ("postprocess" %in% names(add.args)) posp <- eval(add.args[["postprocess"]])

    if ("sep_in" %in% names(add.args)) prep <- eval(add.args[["sep_in"]])
    if ("sep_out" %in% names(add.args)) posp <- eval(add.args[["sep_out"]])


    # convert

    snakecase::to_any_case(
      lab,
      case = case,
      sep_in = prep,
      sep_out = posp
    )

  } else {
    lab
  }
}
