#' Strip White Spaces
#'
#' The function \code{white} collapse all multiple "spaces" in a single space.
#' By default the function identifies a white space by \code{\\s+} which is
#' a shortcut for \code{[^[:space:]]} i.e. tab, newline, vertical tab, form
#' feed, carriage return, space and possibly other locale-dependent characters.
#' There is option to override the pattern to be use to identify white spaces.

#'
#' @param corpus a compatible object storing documents (actually, list (and
#'               \code{\link{corpus_list}}) of (tokened) documents,
#'               character vectors and \code{\link[tm]{VCorpus}})
#' @param ... Other paramenter
#' @param pattern (chr) A regular expression to be use for detection of
#'                withespace. If \code{NULL} (default), \code{\\s+} is used.
#'
#' @return an object of the same class of input with documents witten with
#'         trimmed whitespaces.
#' @export
#'
#' @examples
#' data(liu_corpus)
#'
#' white(c(' one  two   three    '))
#' white(liu_corpus)
white <- function(corpus, ..., pattern = '\\s+'){
  UseMethod('white')
}

#' @rdname white
#' @export
white.list <- function(corpus, ..., pattern = NULL){
  if (is.null(pattern)) {
    pattern <- '\\s+'
  }

  corpus[] <- purrr::map(corpus, ~ stringr::str_trim(
    stringr::str_replace_all(., pattern, ' ')
  ))

  corpus
}

#' @rdname white
#' @export
white.character <- function(corpus, ..., pattern = NULL){
  if (is.null(pattern)) {
    pattern <- '\\s+'
  }

  corpus[] <- purrr::map_chr(corpus, ~ stringr::str_trim(
    stringr::str_replace_all(., pattern, ' ')
  ))

  corpus
}

#' @rdname white
#' @export
white.VCorpus <- function(corpus, ..., pattern = NULL){
  if (is.null(pattern)) {
    pattern <- '\\s+'
  }

  corpus %>%
    corpora2list %>%
    white.list %>%
    list2corpora
}

#' @rdname white
#' @export
white.default <- function(corpus, ..., pattern = NULL){
  stop('white works only with lists, character vectors and VCorpus')
}
