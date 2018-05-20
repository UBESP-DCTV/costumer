#' Stem words
#'
#' @param corpus a compatible object storing documents (actually, list (and
#'               corpus-list of (tokened) documents,
#'               character vectors and \code{\link[tm]{VCorpus}})
#' @param language (chr) language to pass to package \pkg{Snowballc} to perform
#'  the stemming, default is 'english'.
#'
#' @return an object of the same class of input with documents written with
#'         all words stemmed. Names of stemmed tokens are the original un-stemed
#'         ones
#'
#' @export
#'
#' @examples
#' sample_corpus <- list(
#'   c('this is', 'is a', 'a beautiful', 'beautiful day'),
#'   c('hello word')
#' )
#'
stem <- function (corpus, language = 'english') {
  UseMethod('stem')
}

#' @rdname stem
#' @export
stem.list <- function (corpus, language = 'english') {

  ## For each document, create a list to sub-tokenize each token in single
  ## words, stem them and recollapse in a single token, at the end unlisting
  ## the tokens of each documents to recreate the the original structure
  ## filled by "single-words stemmed" documents list
  #
  corpus[] <- corpus %>%
      purrr::map(tok_token) %>%
      purrr::map(~ purrr::map(., SnowballC::wordStem, language)) %>%
      purrr::map(~ purrr::map(., paste, collapse = ' ')) %>%
      purrr::map(unlist)

  corpus
}

#' @rdname stem
#' @export
stem.character <- function (corpus, language = 'english') {

  ## For each document, create a list to sub-tokenize each token in single
  ## words, stem them and recollapse in a single token, at the end unlisting
  ## the tokens of each documents to recreate the the original structure
  ## filled by "single-words stemmed" documents list
  #
  corpus[] <- corpus %>%
    purrr::map(tok_token) %>%
    purrr::map(~ purrr::map(., SnowballC::wordStem, language)) %>%
    purrr::map(~ purrr::map(., paste, collapse = ' ')) %>%
    unlist

  corpus
}


#' @rdname stem
#' @export
stem.VCorpus <- function (corpus, language = 'english') {
  corpus %>%
    corpora2list %>%
    stem.list(language = language) %>%
    list2corpora
}

#' @rdname stem
#' @export
stem.default <- function (corpus, language = 'english') {
  stop('stem works only with lists, character vectors and VCorpus')
}
