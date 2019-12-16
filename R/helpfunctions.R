data_frame <- function(...) {
  x <- data.frame(..., stringsAsFactors = FALSE)
  rownames(x) <- NULL
  x
}

# do we have a stan-model?
is.stan <- function(x) inherits(x, c("stanreg", "stanfit", "brmsfit"))

# return names of objects passed as ellipses argument
dot_names <- function(dots) unname(unlist(lapply(dots, as.character)))


is_float <- function(x) is.numeric(x) && !all(x %% 1 == 0, na.rm = T)


is.num.fac <- function(x) {
  # check if we have numeric levels
  !anyNA(suppressWarnings(as.numeric(levels(x))))
}


.compact_list <- function(x) x[!sapply(x, function(i) length(i) == 0 || is.null(i) || any(i == "NULL"))]


#' @importFrom stats na.omit
is.num.chr <- function(x, na.rm = FALSE) {
  # check if we have numeric character values only
  if (na.rm) x <- stats::na.omit(x)
  !anyNA(suppressWarnings(as.numeric(x)))
}


#' @importFrom purrr compact
isempty <- function(x, first.only = TRUE) {
  # do we have a valid vector?
  if (!is.null(x)) {
    # if it's a character, check if we have only one element in that vector
    if (is.character(x)) {
      # characters may also be of length 0
      if (length(x) == 0) return(TRUE)
      # else, check all elements of x
      zero_len <- sapply(x, function(y) {
        # zero chars, so empty?
        l <- nchar(y) == 0
        # if 'x' was empty, we have no chars, so zero_len will be integer(0).
        # check this here, because zero_len needs to be logical
        if (length(l) == 0) l <- TRUE
        l
      })
      # return result for multiple elements of character vector
      if (first.only) {
        zero_len <- isTRUE(zero_len[1])
        if (length(x) > 0) x <- x[!is.na(x)][1]
      } else {
        return(unname(zero_len))
      }
      # we have a non-character vector here. check for length
    } else if (is.list(x)) {
      x <- .compact_list(x)
      zero_len <- length(x) == 0
    } else {
      zero_len <- length(x) == 0
    }
  }

  any(is.null(x) || zero_len || all(is.na(x)))
}
