tidy_models <- function(model) {
  if (inherits(model, "gls"))
    tidy_gls_model(model)
  else if (inherits(model, "coxph"))
    tidy_cox_model(model)
  else if (inherits(model, "svyglm.nb"))
    tidy_svynb_model(model)
  else if (inherits(model, "glmmTMB"))
    tidy_glmmTMB_model(model)
  else if (inherits(model, c("hurdle", "zeroinfl")))
    tidy_hurdle_model(model)
  else if (inherits(model, "logistf"))
    tidy_logistf_model(model)
  else if (inherits(model, c("clm", "clmm", "polr")))
    tidy_clm_model(model)
  else if (inherits(model, "vgam"))
    tidy_vgam_model(model)
  else if (inherits(model, "lmerModLmerTest"))
    tidy_lmerTest_model(model)
  else if (inherits(model, "Zelig-relogit"))
    tidy_zelig_model(model)
  else
    tidy_generic(model)
}



#' @importFrom broom tidy
tidy_generic <- function(model) {
  # tidy the model
  tryCatch(
    {
      broom::tidy(model, conf.int = FALSE, effects = "fixed")
    },
    error = function(x) { NULL },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )
}


#' @importFrom tibble rownames_to_column
#' @importFrom dplyr select
tidy_lmerTest_model <- function(model) {
  # tidy the model
  tryCatch(
    {
      x <- summary(model)$coef %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "term") %>%
        dplyr::select(1, 2, 3, 5)

      colnames(x) <- c("term", "estimate", "std.error", "statistic")
      x
    },
    error = function(x) { NULL },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )
}


#' @importFrom stats coef
#' @importFrom tibble tibble
tidy_svynb_model <- function(model) {
  if (!isNamespaceLoaded("survey"))
    requireNamespace("survey", quietly = TRUE)

  # keep original value, not rounded
  est <- stats::coef(model)

  tibble::tibble(
    term = substring(names(est), 5),
    estimate = est
  )
}


#' @importFrom broom tidy
tidy_cox_model <- function(model) {
  broom::tidy(model)
}


#' @importFrom tibble rownames_to_column
tidy_gls_model <- function(model) {
  as.data.frame(summary(model)$tTable) %>%
    tibble::rownames_to_column("term")
}


#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
tidy_glmmTMB_model <- function(model) {

  if (!requireNamespace("glmmTMB", quietly = TRUE))
    stop("Package `glmmTMB` required. Please install!", call. = F)

  est <- glmmTMB::fixef(model)

  cond <- tibble::tibble(
    term = names(est[[1]]),
    estimate = est[[1]],
    wrap.facet = "Conditional Model"
  )

  if (length(est[[2]]) > 0) {
    zi <- tibble::tibble(
      term = names(est[[1]]),
      estimate = est[[2]],
      wrap.facet = "Zero-Inflated Model"
    )

    cond <- dplyr::bind_rows(cond, zi)
  }

  cond
}


#' @importFrom tibble rownames_to_column
#' @importFrom purrr map_df
tidy_hurdle_model <- function(model) {
  purrr::map_df(
    summary(model)$coefficients,
    ~ .x %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "term")
  )
}


#' @importFrom tibble tibble
tidy_logistf_model <- function(model) {
  tibble::tibble(
    term = model$terms,
    estimate = model$coefficients
  )
}


#' @importFrom tibble rownames_to_column has_name
#' @importFrom rlang .data
#' @importFrom dplyr select
tidy_clm_model <- function(model) {
  if (!tibble::has_name(model, "coefficients")) {
    smry <- summary(model)
  } else {
    smry <- model
  }

  # get estimates, as data frame
  est <- smry$coefficients %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "term")

  # proper column names
  colnames(est)[1:2] <- c("term", "estimate")

  dplyr::select(est, .data$term, .data$estimate)
}


#' @importFrom stats coef
#' @importFrom tibble tibble
tidy_vgam_model <- function(model) {
  tibble::tibble(term = names(stats::coef(model)))
}

#' @importFrom stats coef qnorm
tidy_zelig_model <- function(model) {
  if (!requireNamespace("Zelig"))
    stop("Package `Zelig` required. Please install", call. = F)

  tibble::tibble(term = names(Zelig::coef(model)))
}
