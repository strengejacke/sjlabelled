#' @title Copy value and variable labels to (subsetted) data frames
#' @name copy_labels
#'
#' @description Subsetting-functions usually drop value and variable labels from
#'                subsetted data frames (if the original data frame has value and variable
#'                label attributes). This function copies these value and variable
#'                labels back to subsetted data frames that have been subsetted, for instance,
#'                with \code{\link{subset}}.
#'
#' @param df_new The new, subsetted data frame.
#' @param df_origin The original data frame where the subset (\code{df_new}) stems from;
#'          use \code{NULL}, if value and variable labels from \code{df_new} should be removed.
#' @return Returns \code{df_new} with either removed value and variable label attributes
#'           (if \code{df_origin = NULL}) or with copied value and variable label
#'           attributes (if \code{df_origin} was the original subsetted data frame).
#'
#' @note In case \code{df_origin = NULL}, all possible label attributes
#'         from \code{df_new} are removed.
#'
#' @examples
#' library(dplyr)
#' data(efc)
#'
#' # create subset - drops label attributes
#' efc.sub <- subset(efc, subset = e16sex == 1, select = c(4:8))
#' str(efc.sub)
#'
#' # copy back attributes from original dataframe
#' efc.sub <- copy_labels(efc.sub, efc)
#' str(efc.sub)
#'
#' # remove all labels
#' efc.sub <- copy_labels(efc.sub)
#' str(efc.sub)
#'
#' # create subset - drops label attributes
#' efc.sub <- subset(efc, subset = e16sex == 1, select = c(4:8))
#' # create subset with dplyr's select - attributes are preserved
#' efc.sub2 <- select(efc, c160age, e42dep, neg_c_7, c82cop1, c84cop3)
#'
#' # copy labels from those columns that are available
#' copy_labels(efc.sub, efc.sub2) %>% str()
#'
#' @export
copy_labels <- function(df_new, df_origin = NULL) {
  # check if old df is NULL. if so, we remove all labels
  # from the data frame.
  if (is.null(df_origin)) {
    # tell user
    message("Removing all variable and value labels from data frame.")
    # remove all labels
    df_new <- remove_all_labels(df_new)
  } else {
    # check params
    if (is.data.frame(df_new) && is.data.frame(df_origin)) {
      # get matching colnames, because we only copy attributes from variables
      # that also exist in the new data frame (of course)
      cn <- intersect(colnames(df_new), colnames(df_origin))

      for (i in cn) {
        # copy variable and value labels
        attr(df_new[[i]], "label") <- attr(df_origin[[i]], "label", exact = TRUE)
        attr(df_new[[i]], "labels") <- attr(df_origin[[i]], "labels", exact = TRUE)
      }
    } else {
      warning("Both `df_origin` and `df_new` must be of class `data.frame`.", call. = F)
    }
  }

  df_new
}
