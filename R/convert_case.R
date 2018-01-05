#' @title Generic case conversion for labels
#' @name convert_case
#'
#' @description This function wraps \code{to_any_case()} from the \pkg{snakecase}
#'   package with certain defaults, used for instance to convert cases in
#'   \code{\link{get_term_labels}}.
#'
#' @param lab Character vector that should be case converted.
#' @param case Desired target case. Labels will automatically converted into the
#'          specified character case. See \code{\link[snakecase]{to_any_case}} for
#'          more details on this argument.
#' @param ... Further arguments passed down to \code{\link[snakecase]{to_any_case}},
#'        like \code{preprocess} or \code{postprocess}.
#'
#' @return \code{lab}, with converted case.
#'
#' @examples
#' data(iris)
#' convert_case(colnames(iris))
#' convert_case(colnames(iris), case = "snake")
#'
#' @importFrom snakecase to_any_case
#' @export
convert_case <- function(lab, case = NULL, ...) {

  if (!is.null(case) && !is.null(lab)) {

    # set defaults

    prep <- "(?<!\\d)\\."
    posp <- " "


    # check additional arguments

    add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

    if ("preprocess" %in% names(add.args)) prep <- eval(add.args[["preprocess"]])
    if ("postprocess" %in% names(add.args)) posp <- eval(add.args[["postprocess"]])


    # convert

    snakecase::to_any_case(
      lab,
      case = case,
      preprocess = prep,
      postprocess = posp
    )

  } else {
    lab
  }
}
