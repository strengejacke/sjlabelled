#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


# do we have a stan-model?
is.stan <- function(x) inherits(x, c("stanreg", "stanfit", "brmsfit"))


#' @importFrom dplyr quos select
get_dot_data <- function(x, qs) {
  if (isempty(qs))
    x
  else
    suppressMessages(dplyr::select(x, !!!qs))
}

# return names of objects passed as ellipses argument
dot_names <- function(dots) unname(unlist(lapply(dots, as.character)))


is_float <- function(x) is.numeric(x) && !all(x %% 1 == 0, na.rm = T)


is_foreign <- function(x) !is.null(x) && x == "value.labels"


is_completely_labelled <- function(x) {
  # get label attribute, which may differ depending on the package
  # used for reading the data
  attr.string <- getValLabelAttribute(x)

  # if variable has no label attribute, use factor levels as labels
  if (is.null(attr.string)) return(TRUE)

  # retrieve named labels
  lab <- attr(x, attr.string, exact = T)
  lab <- lab[!haven::is_tagged_na(lab)]

  if (!is.null(lab) && length(lab) > 0) {
    # get values of variable
    valid.vals <- sort(unique(stats::na.omit(as.vector(x))))
    # retrieve values associated with labels. for character vectors
    # or factors with character levels, these values are character values,
    # else, they are numeric values
    if (is.character(x) || (is.factor(x) && !is.num.fac(x)))
      values <- unname(lab)
    else
      values <- as.numeric(unname(lab))
    # check if we have different amount values than labels
    # or, if we have same amount of values and labels, whether
    # values and labels match or not
    return(length(valid.vals) == length(lab) && !anyNA(match(values, valid.vals)))
  }

  return(TRUE)
}

# auto-detect attribute style for variable labels.
# either haven style ("label") or foreign style
# ("variable.label")
getVarLabelAttribute <- function(x) {
  attr.string <- NULL

  # check if x is data frame. if yes, retrieve one "example" variable
  if (is.data.frame(x) || is.list(x)) {

    # define length for loop
    if (is.data.frame(x))
      counter <- ncol(x)
    else
      counter <- length(x)

    # we need to check all variables until first variable
    # that has any attributes at all - SPSS variables without
    # labels would return NULL, so if -e.g.- first variable
    # of data set has no label attribute, but second had, this
    # function would stop after first attribute and return NULL
    for (i in seq_len(counter)) {
      # retrieve attribute names
      an <- names(attributes(x[[i]]))
      # check for label attributes
      if (any(an == "label") || any(an == "variable.label")) {
        x <- x[[i]]
        break
      }
    }
  }

  # check if vector has label attribute
  if (!is.null(attr(x, "label", exact = T)))
    attr.string <- "label"
  else if (!is.null(attr(x, "variable.label", exact = T)))
    attr.string <- "variable.label"
  else if (is.null(attr.string))
    # not found any label yet?
    attr.string <- "label"

  attr.string
}


# auto-detect attribute style for value labels.
# either haven style ("labels") or foreign style
# ("value.labels")
getValLabelAttribute <- function(x, def.value = NULL) {
  attr.string <- def.value

  # check if x is data frame. if yes, just retrieve one "example" variable
  if (is.data.frame(x)) {
    # find first variable with labels or value.labels attribute
    for (i in seq_len(ncol(x))) {
      # has any attribute?
      if (!is.null(attr(x[[i]], "labels", exact = T)))
        return("labels")
      else if (!is.null(attr(x[[i]], "value.labels", exact = T)))
        return("value.labels")
    }
  } else {
    # check if vector has labels attribute
    if (!is.null(attr(x, "labels", exact = T)))
      attr.string <- "labels"
    else if (!is.null(attr(x, "value.labels", exact = T)))
      attr.string <- "value.labels"
  }

  attr.string
}


is.num.fac <- function(x) {
  # check if we have numeric levels
  !anyNA(suppressWarnings(as.numeric(levels(x))))
}


is.num.chr <- function(x) {
  # check if we have numeric character values only
  !anyNA(suppressWarnings(as.numeric(x)))
}

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
        return(unname(zero_len)[1])
      } else {
        return(unname(zero_len))
      }
      # we have a non-character vector here. check for length
    } else {
      zero_len <- length(x) == 0
    }
  }
  return(is.null(x) || zero_len || is.na(x))
}



#' @importFrom snakecase to_any_case
convert_case <- function(lab, case, ...) {

  if (!is.null(case) && !is.null(lab)) {

    # set defaults

    prep <- "(?<!\\d)\\."
    posp <- " "
    prot <- "\\d"


    # check additional arguments

    add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

    if ("preprocess" %in% names(add.args)) prep <- add.args[["preprocess"]]
    if ("postprocess" %in% names(add.args)) posp <- add.args[["postprocess"]]
    if ("protect" %in% names(add.args)) prot <- add.args[["protect"]]


    # convert

    snakecase::to_any_case(
      lab,
      case = case,
      preprocess = prep,
      postprocess = posp,
      protect = prot
    )

  } else {
    lab
  }
}
