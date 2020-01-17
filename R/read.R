#' @title Import data from other statistical software packages
#' @name read_spss
#'
#' @description Import data from SPSS, SAS or Stata, including NA's, value and variable
#'   labels.
#'
#' @seealso Vignette \href{../doc/intro_sjlabelled.html}{Labelled Data and the sjlabelled-Package}.
#'
#' @param path File path to the data file.
#' @param atomic.to.fac Logical, if \code{TRUE}, categorical variables imported
#'    from the dataset (which are imported as \code{atomic}) will be
#'    converted to factors.
#' @param drop.labels Logical, if \code{TRUE}, unused value labels are removed. See
#'   \code{\link{drop_labels}}.
#' @param tag.na Logical, if \code{TRUE}, missing values are imported
#'    as \code{\link[haven]{tagged_na}} values; else, missing values are
#'    converted to regular \code{NA} (default behaviour).
#' @param path.cat Optional, the file path to the SAS catalog file.
#' @param enc The character encoding used for the file. This defaults to the encoding
#'    specified in the file, or UTF-8. Use this argument to override the default
#'    encoding stored in the file.
#' @param verbose Logical, if \code{TRUE}, a progress bar is displayed that indicates
#'    the progress of converting the imported data.
#'
#' @return A data frame containing the imported, labelled data. Retrieve value labels with
#'   \code{\link{get_labels}} and variable labels with \code{\link{get_label}}.
#'
#' @note These are wrapper functions for \CRANpkg{haven}'s \code{read_*}-functions.
#'
#' @details These read-functions behave slightly differently from \pkg{haven}'s
#'   read-functions:
#'   \itemize{
#'     \item The vectors in the returned data frame are of class \code{atomic}, not of class \code{labelled}. The labelled-class might cause issues with other packages.
#'     \item When importing SPSS data, variables with user defined missings \emph{won't} be read into \code{labelled_spss} objects, but imported as \emph{tagged NA values}.
#'   }
#'   The \code{atomic.to.fac} option only
#'   converts those variables into factors that are of class \code{atomic} and
#'   which have value labels after import. Atomic vectors without value labels
#'   are considered as continuous and not converted to factors.
#'
#' @examples
#' \dontrun{
#' # import SPSS data set. uses haven's read function
#' mydat <- read_spss("my_spss_data.sav")
#'
#' # use haven's read function, convert atomic to factor
#' mydat <- read_spss("my_spss_data.sav", atomic.to.fac = TRUE)
#'
#' # retrieve variable labels
#' mydat.var <- get_label(mydat)
#'
#' # retrieve value labels
#' mydat.val <- get_labels(mydat)}
#' @importFrom haven read_sav read_sas read_dta
#' @export
read_spss <- function(path, atomic.to.fac = FALSE, drop.labels = FALSE, tag.na = FALSE, enc = NULL, verbose = FALSE) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Package 'haven' required for this function. Please install it.")
  }
  # read data file
  data.spss <- haven::read_sav(file = path, encoding = enc, user_na = tag.na)
  # prepare tagged NA?
  if (tag.na) {
    # remember all-NA values
    all_missings <- c()
    # convert NA for all variables
    for (i in seq_len(ncol(data.spss))) {
      # get variable
      x <- data.spss[[i]]

      # has variable ONLY missings?
      if (all(is.na(x))) {
        all_missings <- c(all_missings, i)
      } else {
        # get NA values
        na.values <- attr(x, "na_values", exact = TRUE)
        na.range <- attr(x, "na_range", exact = TRUE)

        # has any NA values?
        if (!is.null(na.values)) {
          # get label attr
          labels <- attr(x, "labels", exact = TRUE)

          # create tagged NA
          tna <- haven::tagged_na(as.character(na.values))

          # replace values with tagged NA
          for (j in seq_len(length(na.values))) {
            x[x == na.values[j]] <- tna[j]
          }

          # do we have any labels?
          if (!is.null(labels)) {
            # get missing labels
            na.val.labels <- names(labels)[labels %in% na.values]

            # do we have any labels for missings? then name tagged
            # NA with value labels, else use values as labels
            empty_val_labels <- isempty(na.val.labels)

            if (length(na.val.labels) > 0 && !empty_val_labels)
              names(tna) <- na.val.labels
            else
              names(tna) <- na.values

            # add/replace value labeld for tagged NA
            labels <- c(labels[!labels %in% na.values], tna)
          } else {
            # use values as names, if we don't have value labels
            names(tna) <- na.values
            labels <- tna
          }
          # set back attribute
          attr(x, "labels") <- labels
        }

        # do we have NA range?
        if (!is.null(na.range)) {
          # check if any of the missing range values actually exists in data
          min.range.start <- min(na.range[!is.infinite(na.range)], na.rm = T)
          max.range.end <- max(na.range[!is.infinite(na.range)], na.rm = T)

          # we start with range up to highest value
          if (any(na.range == Inf) && min.range.start <= max(x, na.rm = TRUE)) {
            x <- set_na(x, na = sort(stats::na.omit(unique(x[x >= min.range.start]))), as.tag = TRUE)
          }

          # we start with range up to highest value
          if (any(na.range == -Inf) && max.range.end >= min(x, na.rm = TRUE)) {
            x <- set_na(x, na = sort(stats::na.omit(unique(x[x <= max.range.end]))), as.tag = TRUE)
          }

          # here we have no infinite value range
          if (!any(is.infinite(na.range))) {
            x <- set_na(x, na = sort(stats::na.omit(unique(c(
              na.range[!is.infinite(na.range)], x[x >= min.range.start & x <= max.range.end]
            )))), as.tag = TRUE)
          }
        }

        # finally, copy x back to data frame
        if (!is.null(na.range) || !is.null(na.values)) data.spss[[i]] <- x
      }
    }

    # do we have any "all-missing-variables"?
    if (!isempty(all_missings)) {
      message(sprintf("Following %i variables have only missing values:", length(all_missings)))
      cat(paste(all_missings, collapse = ", "))
      cat("\n")
    }
  }

  .read_postprocessing(data.spss, atomic.to.fac, drop.labels, verbose)
}


#' @rdname read_spss
#' @export
read_sas <- function(path, path.cat = NULL, atomic.to.fac = FALSE, drop.labels = FALSE, enc = NULL, verbose = FALSE) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Package 'haven' required for this function. Please install it.")
  }
  # read data file
  data <- haven::read_sas(data_file = path, catalog_file = path.cat, encoding = enc)

  # find all-NA values
  len <- nrow(data)
  all_missings <- names(which(unlist(lapply(data, function(x) sum(is.na(x)) == len)) == TRUE))

  # do we have any "all-missing-variables"?
  if (!isempty(all_missings)) {
    message(sprintf("Following %i variables have only missing values:", length(all_missings)))
    cat(paste(all_missings, collapse = ", "))
    cat("\n")
  }

  .read_postprocessing(data, atomic.to.fac, drop.labels, verbose)
}


#' @rdname read_spss
#' @export
read_stata <- function(path, atomic.to.fac = FALSE, drop.labels = FALSE, enc = NULL, verbose = FALSE) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Package 'haven' required for this function. Please install it.")
  }
  # read data file
  data <- haven::read_dta(file = path, encoding = enc)

  # find all-NA values
  len <- nrow(data)
  all_missings <- names(which(unlist(lapply(data, function(x) sum(is.na(x)) == len)) == TRUE))

  # do we have any "all-missing-variables"?
  if (!isempty(all_missings)) {
    message(sprintf("Following %i variables have only missing values:", length(all_missings)))
    cat(paste(all_missings, collapse = ", "))
    cat("\n")
  }

  .read_postprocessing(data, atomic.to.fac, drop.labels, verbose)
}


.read_postprocessing <- function(data, atomic.to.fac, drop.labels, verbose) {
  # remove label attributes
  d <- unlabel(data, verbose = verbose)

  # drop unused labels
  if (drop.labels) d <- drop_labels(d)

  # convert atomic values to factors
  if (atomic.to.fac) d <- .atomic_to_fac(d)

  # return data frame
  d
}



# converts atomic numeric vectors into factors with
# numerical factor levels
.atomic_to_fac <- function(d) {
  # tell user...
  message("Converting atomic to factors. Please wait...\n")
  # iterate all columns

  as.data.frame(lapply(
    d,
    function(x) {
      # capture value labels attribute first
      labs <- attr(x, "labels", exact = T)
      # and save variable label, if any
      lab <- attr(x, "label", exact = T)
      # is atomic, which was factor in SPSS?
      if (is.atomic(x) && !is.null(labs) && length(labs) >= length(unique(stats::na.omit(x)))) {
        # so we have value labels (only typical for factors, not
        # continuous variables) and a variable of type "atomic" (SPSS
        # continuous variables would be imported as numeric) - this
        # indicates we have a factor variable. now we convert to
        # factor
        x <- as.factor(x)
        # set back labels attribute
        attr(x, "labels") <- labs
        # any variable label?
        if (!is.null(lab)) attr(x, "label") <- lab
      }
      x
    }), stringsAsFactors = FALSE)
}


#' @importFrom tools file_ext
#' @rdname read_spss
#' @export
read_data <- function(path, atomic.to.fac = FALSE, drop.labels = FALSE, enc = NULL, verbose = FALSE) {
  switch(
    tools::file_ext(path),
    "sav" = ,
    "por" = read_spss(path = path, atomic.to.fac = atomic.to.fac, drop.labels = drop.labels, enc = enc, verbose = verbose),
    "dta" = read_stata(path = path, atomic.to.fac = atomic.to.fac, drop.labels = drop.labels, enc = enc, verbose = verbose),
    read_sas(path = path, atomic.to.fac = atomic.to.fac, drop.labels = drop.labels, enc = enc, verbose = verbose)
  )
}