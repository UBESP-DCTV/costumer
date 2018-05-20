#' Variable selector (best tfidf rank)
#'
#' @param original_train original (\code{\link[slam]{simple_triplet_matrix}})
#'        with overall variables
#' @param max_retained the maximum number of variable to retain
#'        (default is
#'        \code{min(1000L, as.integer(ncol(original) * 10/100))})
#'
#' @return an object of the same class of \code{original} with only the
#'         top tifidf max_retained token in the sum of relative tfidf
#' @export
#'
#' @examples
#' data(liu_dtm)
#' variable_select(liu_dtm)
variable_select <- function(original_train,
  max_retained = min(1000L, as.integer(ncol(original_train) * 10/100))
) {

  idx <- original_train %>%
    tfidf4dtm() %>%
    slam::col_sums() %>%
    order(decreasing = TRUE)

  used_train <- original_train[, idx[seq_len(max_retained)]]
  attributes(used_train) <- attributes(original_train)
  used_train
}
