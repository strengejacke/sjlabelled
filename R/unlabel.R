#' @title Convert labelled vectors into normal classes
#' @name unlabel
#'
#' @description This function converts \code{\link[haven]{labelled}} class vectors
#'    into a generic data format, which means that simply all \code{\link[haven]{labelled}}
#'    class attributes will be removed, so all vectors / variables will most
#'    likely become \code{\link{atomic}}.
#'
#' @param x A data frame, which contains \code{\link[haven]{labelled}} class
#'    vectors or a single vector of class \code{labelled}.
#'
#' @inheritParams read_spss
#'
#' @return A data frame or single vector (depending on \code{x}) with common object classes.
#'
#' @note This function is currently only used to avoid possible compatibility issues
#'    with \code{\link[haven]{labelled}} class vectors. Some known issues with
#'    \code{\link[haven]{labelled}} class vectors have already been fixed, so
#'    it might be that this function will become redundant in the future.
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
unlabel <- function(x, verbose = TRUE) {
  # check if complete data frame or only single
  # vector should be converted
  if (is.data.frame(x)) {
    # create progress bar
    if (verbose)
      pb <- utils::txtProgressBar(min = 0, max = ncol(x), style = 3)
    else
      pb <- NULL

    # tell user...
    if (verbose) message("Converting labelled-classes. Please wait...\n")

    for (i in seq_len(ncol(x))) {
      # remove labelled class
      if (is_labelled(x[[i]])) x[[i]] <- unclass(x[[i]])
      # update progress bar
      if (verbose) utils::setTxtProgressBar(pb, i)
    }

    if (!is.null(pb)) close(pb)

    # remove redundant class attributes
    x <- as.data.frame(x, stringsAsFactors = FALSE)
  } else {
    # remove labelled class
    if (is_labelled(x)) x <- unclass(x)
  }

  x
}
