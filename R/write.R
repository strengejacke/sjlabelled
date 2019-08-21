#' @title Write data to other statistical software packages
#' @name write_spss
#'
#' @description These functions write the content of a data frame to an SPSS, SAS or
#'                Stata-file.
#'
#' @param x A data frame that should be saved as file.
#' @param path File path of the output file.
#' @param version File version to use. Supports versions 8-14.
#' @param drop.na Logical, if \code{TRUE}, tagged \code{NA} values with value labels
#'          will be converted to regular NA's. Else, tagged \code{NA} values will be replaced
#'          with their value labels. See 'Examples' and \code{\link{get_na}}.
#'
#' @export
write_spss <- function(x, path, drop.na = FALSE) {
  .write_data(x = x, path = path, type = "spss", version = 14, drop.na = drop.na)
}


#' @rdname write_spss
#' @export
write_stata <- function(x, path, drop.na = FALSE, version = 14) {
  .write_data(x = x, path = path, type = "stata", version = version, drop.na = drop.na)
}


#' @rdname write_spss
#' @export
write_sas <- function(x, path, drop.na = FALSE) {
  .write_data(x = x, path = path, type = "sas", version = 14, drop.na = drop.na)
}


#' @importFrom purrr map
#' @importFrom haven write_sav write_dta write_sas
.write_data <- function(x, path, type, version, drop.na) {
  # make sure to have tidy labels
  message("Tidying value labels. Please wait...")
  x <- tidy_labels(x)

  # convert data to labelled
  # x <- as_label(x, add.non.labelled = T, drop.na = drop.na)
  x <- as_labelled(x, add.labels = TRUE, skip.strings = TRUE)

  # check for correct column names
  for (i in seq_len(ncol(x))) {
    # check column name
    end.point <- colnames(x)[i]
    # if it ends with a dot, add a char. dot is invalid last char for SPSS
    if (substr(end.point, nchar(end.point), nchar(end.point)) == ".") {
      colnames(x)[i] <- paste0(end.point, i)
    }
  }

  # tell user
  message(sprintf("Writing %s file to '%s'. Please wait...", type, path))

  if (type == "spss") {
    # write SPSS
    haven::write_sav(data = x, path = path)
  } else if (type == "stata") {
    # write Stata
    haven::write_dta(data = x, path = path, version = version)
  } else if (type == "sas") {
    # write Stata
    haven::write_sas(data = x, path = path)
  }
}
