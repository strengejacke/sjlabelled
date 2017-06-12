#' @title Convert labelled vectors into normal classes
#' @name unlabel
#'
#' @description This function converts \code{\link[haven]{labelled}} class vectors
#'                into a generic data format, which means that simply all \code{\link[haven]{labelled}}
#'                class attributes will be removed, so all vectors / variables will most
#'                likely become \code{\link{atomic}}.
#'
#' @param x A data frame, which contains \code{\link[haven]{labelled}} class
#'          vectors or a single vector of class \code{labelled}.
#'
#' @return A data frame or single vector (depending on \code{x}) with common object classes.
#'
#' @note This function is currently only used to avoid possible compatibility issues
#'         with \code{\link[haven]{labelled}} class vectors. Some known issues with
#'         \code{\link[haven]{labelled}} class vectors have already been fixed, so
#'         it might be that this function will become redundant in the future.
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom tibble as_tibble
#' @export
unlabel <- function(x) {
  # check if complete data frame or only single
  # vector should be converted
  if (is.data.frame(x)) {
    # create progress bar
    pb <- utils::txtProgressBar(min = 0, max = ncol(x), style = 3)

    # tell user...
    message("Converting labelled-classes. Please wait...\n")

    for (i in seq_len(ncol(x))) {
      # remove labelled class
      if (is_labelled(x[[i]])) x[[i]] <- unclass(x[[i]])
      # update progress bar
      utils::setTxtProgressBar(pb, i)
    }

    close(pb)

    # remove redundant class attributes
    x <- tibble::as_tibble(x)
  } else {
    # remove labelled class
    if (is_labelled(x)) x <- unclass(x)
  }

  x
}
