#' Lowering
#'
#' @param corpus a compatible object storing documents (actually, list (and
#' \code{\link{corpus_list}}) of (tokened) documents, character
#' vectors and \code{\link[tm]{VCorpus}})
#'
#' @return an object of the same class of input with documents witten with only
#'         lowercase letters.
#' @export
#'
#' @examples
#' data(liu_corpus)
#' lowering(c('Hello', 'World'))
#' lowering(liu_corpus)$content[[1]]$content
lowering <- function(corpus) {
  UseMethod('lowering')
}

#' @rdname lowering
#' @export
lowering.list <- function(corpus) {
  corpus[] <- purrr::map(corpus, ~ tolower(.))

  corpus
}

#' @rdname lowering
#' @export
lowering.character <- function(corpus) {
  corpus[] <- tolower(corpus)

  corpus
}

#' @rdname lowering
#' @export
lowering.VCorpus <- function(corpus) {
  corpus %>%
    corpora2list %>%
    lowering.list %>%
    list2corpora
}

#' @rdname lowering
#' @export
lowering.default <- function(corpus) {
  stop('lowering works only with lists, character vectors and VCorpus')
}


