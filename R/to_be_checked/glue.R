#' Glue character strings
#'
#' \code{glue} collapse a character vector in a single field eliminating missing
#' values.
#'
#' @param texts (chr) character vector to collapse
#' @param collapse (chr, default = ' ') character string to separate the results
#'
#' @return character vector
#' @export
#'
glue <- function (txt_vec, collapse = ' ') {
    paste(na.omit(c(texts)[c(texts)!=""]), collapse = collapse)
}


#' Glue unique elements
#'
#' \code{glunique} collapse a character vector in a single field, eliminanting
#' missing values and duplicates elements (of the original character vector)
#'
#' @param text (chr) character vector to collapse
#'
#' @return character vector
#' @export
#'
glunique <- function (x) {
    glue(unique(x))
}
