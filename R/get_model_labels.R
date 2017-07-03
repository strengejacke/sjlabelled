#' @title Retrieve labels of model terms from regression models
#' @name get_term_labels
#'
#' @description This function retrieves tagged NA values and their associated
#'                value labels from a labelled vector.
#'
#' @param models One or more fitted regression models. May also be glm's or
#'        mixed models. Any model with \code{\link[stats]{model.frame}} method
#'        and which is supported by \pkg{broom}'s \code{\link[broom]{tidy}}
#'        function should work.
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
#' # get label of dv
#' get_dv_labels(fit)
#'
#' # create "labelled" plot
#' library(ggplot2)
#' library(broom)
#' ggplot(tidy(fit)[-1, ], aes(x = term, y = estimate)) +
#'   geom_point() +
#'   coord_flip() +
#'   scale_x_discrete(labels = get_term_labels(fit))
#'
#' @importFrom purrr map flatten_chr
#' @importFrom broom tidy
#' @importFrom stats model.frame
#' @importFrom dplyr select slice
#' @export
get_term_labels <- function(models) {
  # to be generic, make sure argument is a list
  if (!inherits(models, "list")) models <- list(models)

  # get model terms and model frame
  m <- purrr::map(models, ~ dplyr::slice(broom::tidy(.x, effects = "fixed"), -1))
  mf <- purrr::map(models, ~ dplyr::select(stats::model.frame(.x), -1))

  # get all variable labels for predictors
  lbs1 <- purrr::map(1:length(m), function(x) {
    terms <- unique(m[[x]]$term)
    get_label(mf[[x]], def.value = terms)
  }) %>% unlist()

  # for categorical predictors, we have one term per
  # value (factor level), so extract these as well
  lbs2 <- purrr::map(mf, ~ purrr::map(.x, function(x) {
    if (is.factor(x))
      get_labels(x)
  }) %>% unlist())

  # flatten, if we have any elements. in case all predictors
  # were non-factors, list has only NULLs
  lbs2 <- if (!is.null(unlist(lbs2)))
    purrr::flatten_chr(lbs2)
  else
    NULL

  # remove duplicated
  lbs <- c(lbs1, lbs2)
  lbs <- lbs[!duplicated(lbs)]

  # set default names for values
  if (is.null(names(lbs))) names(lbs) <- lbs

  # the vector now contains all possible labels, as named vector.
  # since ggplot uses named vectors as labels for axis-scales, matching
  # of labels is done automatically
  lbs
}


#' @rdname get_term_labels
#' @importFrom purrr map map2_chr
#' @importFrom dplyr pull
#' @importFrom stats model.frame
#' @export
get_dv_labels <- function(models) {
  # to be generic, make sure argument is a list
  if (!inherits(models, "list")) models <- list(models)

  # get intercept vectors
  intercepts <- purrr::map(models, ~ dplyr::pull(stats::model.frame(.x), var = 1))
  intercepts.names <- purrr::map(models, ~ deparse(stats::formula(.x)[[2L]]))

  # get all labels
  lbs <- purrr::map2_chr(
    intercepts,
    intercepts.names,
    ~ get_label(.x, def.value = .y)
  )

  lbs
}
