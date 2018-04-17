#' Check Tokenization for list
#'
#' This functions check if documents into a corpora are stored as single string
#' or are tokened
#'
#' @param x An object containing documents
#'
#' @return A logical vector
#' @export
#'
#' @examples
#' data(liu_corpus)
#' check_tokened(liu_corpus)
#'
check_tokened <- function(x) {
  UseMethod('check_tokened')
}

#' @rdname check_tokened
#' @export
check_tokened.list <- function(x) {
  !all(purrr::map_int(x, ~ sum(length(.))) == 1)
}

#' @rdname check_tokened
#' @export
check_tokened.corpus_list <- function(x) {
  !all(purrr::map_int(x, ~ sum(length(.))) == 1)
}

#' @rdname check_tokened
#' @export
check_tokened.character <- function(x) {
  FALSE
}

#' @rdname check_tokened
#' @export
check_tokened.VCorpus <- function(x) {
  x <- purrr::map(x$content, ~ .$content)
  !all(purrr::map_int(x, ~ sum(length(.))) == 1)
}

#' @rdname check_tokened
#' @export
check_tokened.default <- function(x) {
  stop('check_tokened() can be run only on lists, corpus_lists, character vectors or VCorpus')
}

