#' @title Deprecated functions
#' @name set_note
#'
#' @description A list of deprecated functions.
#'
#' @param ... Not used.
#' @return Nothing.
#'
#' @export
set_note <- function(...) {
  .Deprecated("comment", package = "base", msg = "`set_note()` is defunct. Please use `base::comment()` instead.")
}

#' @rdname set_note
get_note <- function(...) {
  .Deprecated("comment", package = "base", msg = "`get_note()` is defunct. Please use `base::comment()` instead.")
}
