#' @rdname set_label
#' @export
var_labels <- function(x, ...) {
  # get dots
  .dots <- match.call(expand.dots = FALSE)$`...`

  if (inherits(.dots, "pairlist")) {
    if (!requireNamespace("rlang", quietly = TRUE)) {
      stop("Package 'rlang' required for this function to work. Please install it.")
    }
    .dots <- lapply(rlang::ensyms(...), rlang::as_string) %>% unlist()
  } else {
    .dots <- unlist(.dots)
  }

  # select variables
  vars <- names(.dots)
  # get new labels
  labels <- unname(.dots)

  # non-matching column names
  non.vars <- which(!(vars %in% colnames(x)))

  # check if all variables exist in data frame
  if (!isempty(non.vars)) {
    # tell user
    warning(sprintf(
      "Following elements are no valid column names in `x`: %s",
      paste(vars[non.vars], collapse = ",")
    ),
    call. = F)
    # remove invalid names
    vars <- vars[-non.vars]
    labels <- labels[-non.vars]
  }

  # set label for all variables
  for (i in seq_len(length(vars))) {
    attr(x[[vars[i]]], "label") <- labels[i]
  }

  # return data
  x
}
