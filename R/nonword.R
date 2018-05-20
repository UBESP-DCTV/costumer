#' Eliminate non-words
#'
#' This function's aim is to eliminate everything is not an alphanumeric
#' word/token from a corpora of documents. It also has an option to decide if
#' numbers has to be removed too. Moreover, it is possible to override both
#' the paramenter for the pattern identifying words and the one identifying
#' the replacements (default is a white space).
#'
#'
#' @param corpus a compatible object storing documents (actually, list (and
#'               corpus-list of (tokened) documents,
#'               character vectors and \code{\link[tm]{VCorpus}})
#' @param numbers (lgl) if TRUE also numbers are removed (default FALSE)
#' @param ... Additional option
#' @param pattern (chr) an alternative regular expression used to remove
#'                (i.e., to substitute with \code{replacement}) everything
#'                that match it. Default is \code{NULL}. If not \code{NULL}
#'                the option numbers is ignored.
#' @param replacement (chr) the string used to sobstitute the ones which will
#'                    be eliminated. Default is \code{' '}.
#'
#' @return an object of the same class of input with documents witten with only
#'         "words" retained.
#' @export
#'
#' @examples
#' data(liu_corpus)
#'
#' nonword('hell0 w.rld')
#' nonword('hell0 w.rld', numbers = TRUE)                  # remove also numbers
#' nonword('hell0 w.rld', replacement = '*')    # use "*" instead of white space
#' nonword('hell0 w.rld', pattern = 'w[^\\s]+')     # anithing starting with "w"
#'
#' nonword(liu_corpus)$content[[1]]$content # "-" removed in "anti-angiogenesis"
nonword <- function (corpus, numbers = FALSE, ...,
    pattern = NULL, replacement = ' '
  ) {
  UseMethod('nonword')
}

#' @rdname nonword
#' @export
nonword.list <- function(corpus, numbers = FALSE, ...,
    pattern = NULL, replacement = ' '
  ) {

  if (!numbers) {
    if (is.null(pattern)) {
      pattern <- '\\W'
    }
    corpus[] <- purrr::map(corpus,
      ~ stringr::str_replace_all(., pattern, replacement)
    )
  } else {
    if (is.null(pattern)) {
      pattern <- '[^[:alpha:]_]'
    }
    corpus[] <- purrr::map(corpus,
      ~ stringr::str_replace_all(., pattern, replacement)
    )
  }
  corpus
}

#' @rdname nonword
#' @export
nonword.VCorpus <- function(corpus, numbers = FALSE, ...,
    pattern = NULL, replacement = ' '
  ) {
  corpus %>%
    corpora2list %>%
    nonword.list(numbers = numbers, ..., pattern = pattern,
      replacement = replacement
    ) %>%
    list2corpora
}

#' @rdname nonword
#' @export
nonword.character <- function(corpus, numbers = FALSE, ...,
    pattern = NULL, replacement = ' '
  ) {
  if (!numbers) {
    if (is.null(pattern)) {
      pattern <- '\\W'
    }
    corpus[] <- purrr::map_chr(corpus,
      ~ stringr::str_replace_all(., pattern, replacement)
    )
  } else {
    if (is.null(pattern)) {
      pattern <- '[^[:alpha:]_]'
    }
    corpus[] <- purrr::map_chr(corpus,
      ~ stringr::str_replace_all(., pattern, replacement)
    )
  }
  corpus
}


#' @rdname nonword
#' @export
nonword.default <- function(corpus, numbers = FALSE, ...,
    pattern = NULL, replacement = ' '
  ) {
  stop('nonwords works only with lists, character vectors and VCorpus')
}
