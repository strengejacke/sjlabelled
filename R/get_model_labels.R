#' @title Retrieve labels of model terms from regression models
#' @name get_term_labels
#'
#' @description This function retrieves variable labels from model terms. In case
#'        of categorical variables, where one variable has multiple dummies,
#'        variable name and category value is returned.
#'
#' @param models One or more fitted regression models. May also be glm's or
#'        mixed models. Any model with \code{\link[stats]{model.frame}} method
#'        and which is supported by \pkg{broom}'s \code{\link[broom]{tidy}}
#'        function should work.
#' @param mark.cat Logical, if \code{TRUE}, the returned vector has an
#'        attribute with logical values, which indicate whether a label indicates
#'        the value from a factor category (attribute value is \code{TRUE}) or
#'        a term's variable labels (attribute value is \code{FALSE}).
#' @param case Desired target case. Labels will automatically converted into the
#'          specified character case. See \code{\link[snakecase]{to_any_case}} for
#'          more details on this argument.
#' @param prefix Indicates whether the value labels of categorical variables
#'          should be prefixed, e.g. with the variable name or variable label.
#'          May be abbreviated. See 'Examples',
#' @param mv,multi.resp Logical, if \code{TRUE} and \code{models} is a multivariate
#'          response model from a \code{brmsfit} object, then the labels for each
#'          dependent variable (multiple responses) are returned.
#' @param ... Further arguments passed down to \code{\link[snakecase]{to_any_case}},
#'        like \code{preprocess} or \code{postprocess}.
#'
#' @return For \code{get_term_labels()}, a (named) character vector with
#'         variable labels of all model terms, which can be used, for instance,
#'         as axis labels to annotate plots. \cr \cr For \code{get_dv_labels()},
#'         a character vector with variable labels from all dependent variables
#'         of \code{models}.
#'
#' @details Typically, the variable labels from model terms are returned. However,
#'          for categorical terms that have estimates for each category, the
#'          value labels are returned as well. As the return value is a named
#'          vector, you can easily use it with \pkg{ggplot2}'s \code{scale_*()}
#'          functions to annotate plots.
#'
#' @examples
#' # use data set with labelled data
#' data(efc)
#'
#' fit <- lm(barthtot ~ c160age + c12hour + c161sex + c172code, data = efc)
#' get_term_labels(fit)
#'
#' # make "education" categorical
#' library(sjmisc)
#' efc$c172code <- to_factor(efc$c172code)
#' fit <- lm(barthtot ~ c160age + c12hour + c161sex + c172code, data = efc)
#' get_term_labels(fit)
#'
#' # prefix value of categorical variables with variable name
#' get_term_labels(fit, prefix = "varname")
#'
#' # prefix value of categorical variables with value label
#' get_term_labels(fit, prefix = "label")
#'
#' # get label of dv
#' get_dv_labels(fit)
#'
#' @importFrom purrr map flatten_chr
#' @importFrom insight find_parameters get_data
#' @importFrom stats model.frame coef terms
#' @importFrom dplyr select slice
#' @export
get_term_labels <- function(models, mark.cat = FALSE, case = NULL, prefix = c("none", "varname", "label"), ...) {

  prefix <- match.arg(prefix)

  ## TODO better handling for stanmvreg

  if (inherits(models, "stanmvreg")) {
    return(
      stats::terms(models) %>%
        purrr::map(~ attr(.x, "term.labels")) %>%
        purrr::flatten_chr()
    )
  }

  # to be generic, make sure argument is a list
  if (!inherits(models, "list")) models <- list(models)

  # TODO tidyr for non-supported models


  # get model terms and model frame
  m <- try(purrr::map(models, ~ insight::find_predictors(.x)), silent = TRUE)
  mf <- try(purrr::map(models, ~ dplyr::select(insight::get_data(.x), -1)), silent = TRUE)

  # return NULL on error
  if (inherits(m, "try-error") || inherits(mf, "try-error")) {
    return(NULL)
  }


  # get all variable labels for predictors

  lbs1 <- purrr::map(1:length(m), function(x) {
    if (is.null(mf[[x]])) {
      m[[x]][-1]
    } else {
      get_label(mf[[x]], def.value = colnames(mf[[x]]))
    }
  }) %>% unlist()


  # any empty name? if yes, use label as name

  empty <- nchar(names(lbs1))

  if (any(empty == 0)) {
    empty <- which(empty == 0)
    names(lbs1)[empty] <- lbs1[empty]
  }


  # for categorical predictors, we have one term per
  # value (factor level), so extract these as well

  lbs2 <- purrr::map(mf, ~ purrr::map2(.x, colnames(.x), function(.x, .y) {
    if (is.factor(.x)) {
      l <- get_labels(.x)
      if (!anyNA(suppressWarnings(as.numeric(l))))
        paste0(.y, l)
      else
        l
    }
  }) %>% unlist())

  fixed.names <- purrr::map(mf, ~ purrr::map2(.x, colnames(.x), function(.x, .y) {
    if (is.factor(.x)) paste0(.y, levels(.x))
  }) %>% unlist())

  # flatten, if we have any elements. in case all predictors
  # were non-factors, list has only NULLs

  lbs2 <- if (!is.null(unlist(lbs2)))
    purrr::flatten_chr(lbs2)
  else
    NULL

  fixed.names <- if (!is.null(unlist(fixed.names)))
    purrr::flatten_chr(fixed.names)
  else
    NULL

  names(lbs2) <- unname(fixed.names)

  # create logical to indicate which labels come from factors
  fl1 <- vector(mode = "logical", length = length(lbs1))

  if (!is.null(lbs2)) {
    fl2 <- vector(mode = "logical", length = length(lbs2))
    fl2[1:length(fl2)] <- TRUE
  } else {
    fl2 <- NULL
  }


  # remove duplicated
  lbs <- c(lbs1, lbs2)
  fl <- c(fl1, fl2)

  keep <- !(duplicated(lbs) & duplicated(names(lbs)))

  lbs <- lbs[keep]
  fl <- fl[keep]


  # set default names for values
  if (is.null(names(lbs))) names(lbs) <- lbs

  # do we have partial empty names? if yes, fill them
  en <- which(nchar(names(lbs)) == 0)
  if (!isempty(en)) names(lbs)[en] <- lbs[en]


  # prefix labels
  if (prefix != "none")
    lbs <- prepare.labels(lbs, catval = fl, style = prefix)


  # the vector now contains all possible labels, as named vector.
  # since ggplot uses named vectors as labels for axis-scales, matching
  # of labels is done automatically
  lbs <- convert_case(lbs, case, ...)

  # check if attribute is requested
  if (mark.cat) attr(lbs, "category.value") <- fl

  lbs
}


prepare.labels <- function(x, catval, style = c("varname", "label")) {
  x_var <- names(x[!catval])
  x_val <- names(x[catval])

  for (i in x_var) {
    pos <- string_starts_with(pattern = i, x = x_val)

    if (!isempty(pos) && length(pos) > 0) {
      match.vals <- x_val[pos]
      if (style == "label")
        x[match.vals] <- sprintf("%s: %s", x[i], x[match.vals])
      else
        x[match.vals] <- sprintf("%s: %s", i, x[match.vals])
    }
  }

  x
}


#' @rdname get_term_labels
#' @importFrom purrr map map2 flatten_chr
#' @importFrom dplyr pull select
#' @importFrom stats model.frame
#' @export
get_dv_labels <- function(models, case = NULL, multi.resp = FALSE, mv = FALSE, ...) {

  if (!missing(multi.resp)) mv <- multi.resp

  # to be generic, make sure argument is a list
  if (!inherits(models, "list")) models <- list(models)


  intercepts.names <- tryCatch({
    purrr::map(models, function(x) {
      if (inherits(x, "brmsfit")) {
        if (is.null(stats::formula(x)$formula) && !is.null(stats::formula(x)$responses))
          if (mv)
            stats::formula(x)$responses
          else
            paste(stats::formula(x)$responses, collapse = ", ")
        else
          deparse(stats::formula(x)$formula[[2L]])
      } else if (inherits(x, "stanmvreg")) {
        if (mv)
          purrr::map_chr(stats::formula(x), ~ deparse(.x[[2L]]))
        else
          paste(purrr::map_chr(stats::formula(x), ~ deparse(.x[[2L]])), collapse = ", ")
      } else {
        deparse(stats::formula(x)[[2L]])
      }
    })},
    error = function(x) { NULL },
    warning = function(x) { NULL }
  )


  mf <- tryCatch({
    purrr::map2(
      models,
      intercepts.names,
      function(x, y) {
        m <- insight::get_data(x)
        if (mv && inherits(x, "brmsfit"))
          colnames(m) <- gsub(pattern = "_", replacement = "", x = colnames(m), fixed = TRUE)
        y <- y[obj_has_name(m, y)]
        if (length(y) > 0)
          dplyr::select(m, !! y)
        else
          m[[1]]
      }
    )},
    error = function(x) { NULL },
    warning = function(x) { NULL }
  )


  if (is.null(intercepts.names) || is.null(mf)) {
    return(rep_len("Dependent variable", length.out = length(models)))
  }


  # get all labels

  lbs <- purrr::map2(
    mf,
    intercepts.names,
    ~ get_label(.x, def.value = .y)
  )


  # flatten list, and check for correct elements

  lbs <- purrr::flatten_chr(lbs)


  # There are some formulas that return a rather cryptic
  # name. In such cases, the variable name might have more
  # than 1 element, and here we need to set a proper default

  if (!mv && length(lbs) > length(models)) lbs <- "Dependent variable"

  convert_case(lbs, case, ...)
}
