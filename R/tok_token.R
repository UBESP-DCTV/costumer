#' Tokenizator of token
#'
#' The function's aim is to access tokens at their single-words component level
#' maintaining the information of the original token structure
#'
#' @param doc (chr) A character vector representing a tokenized tocument
#'
#' @return (list) of character vector each element representing
#'         the word-components of each original token
#'
#' @examples
#' tokened_document <- c('this is', 'is a', 'a beautiful', 'beautiful day')
#'
#' tok_token(tokened_document)
tok_token <- function(doc) {

  setNames(
    purrr::map(doc,
      ~ stringi::stri_extract_all_words(.) %>%
        unlist  # do not use "simplify = TRUE" because it returns a 1-row matrix
    ),
    doc
  )
}
