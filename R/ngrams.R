#' n-Gram creators
#'
#' The function aims is to create the ngram tokens for each document in a
#' corpora.
#'
#' @param corpus a compatible object storing documents (actually, list (and
#'               \code{\link{corpus_list}}) of (tokened) documents,
#'               character vectors and \code{\link[tm]{VCorpus}})
#' @param n_min (num) minimum number of words to include in the grams
#' @param n_max (num) maximum number of words to include into the grams
#' @param parallel (lgl) if \code{TRUE} perform the computation in parallel
#'                 using the \code{parallel} package functionality. Default is
#'                 \code{FALSE}.
#'
#'
#' @return an object of the same class of input (except for
#'         \code{character vector} input, for which the output is a \code{list})
#'         with documents tokenized in ngram.
#' @export
#'
ngram <- function (corpus, n_min = 1, n_max = 2, ..., parallel= FALSE,
  ncores = parallel::detectCores() - 1
) {
  UseMethod('ngram')
}

#' @rdname ngram
#' @export
ngram.list <- function(corpus, n_min = 1, n_max = 2, ..., parallel  = FALSE,
  ncores = parallel::detectCores() - 1
) {
  if (!parallel) {
    corpus[] <- purrr::map(corpus,
      ~ RWeka::NGramTokenizer(.,
        control = RWeka::Weka_control(min = n_min, max = n_max)
      )
    )
  } else {
    cl <- parallel::makePSOCKcluster(ncores)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterExport(cl,
      varlist = c('corpus', 'n_min', 'n_max'), envir = environment()
    )

    corpus[] <- parallel::parLapply(cl  = cl,
      X   = corpus,
      fun = function(x) RWeka::NGramTokenizer(x ,
        control = RWeka::Weka_control(min = n_min, max = n_max)
      )
    )
  }
  corpus
}


#' @rdname ngram
#' @export
ngram.VCorpus <- function(corpus, n_min = 1, n_max = 2, ..., parallel  = FALSE,
  ncores = parallel::detectCores() - 1
) {
  corpus %>%
    corpora2list %>%
    ngram.list(
      n_min = n_min, n_max = n_max, ..., parallel = parallel, ncores = ncores
    ) %>%
    list2corpora
}

#' @rdname ngram
#' @export
ngram.character <- function(corpus, n_min = 1, n_max = 2, ...,
  parallel  = FALSE, ncores = parallel::detectCores() - 1,
  docs_or_tokens = c('docs', 'tokens')
) {

  docs_or_tokens = match.arg(docs_or_tokens)

  ## If the vector represents a list of documents convert to it, if represents
  ## a list of tokens of a single documents convert it to a single-element
  ## list
  #
  switch(docs_or_tokens,
    docs   = {
      corpus <- as.list(corpus)
      warning('Pay attention: input is a character vector, output will be a list!')
    },
    tokens = {
      corpus <- list(corpus)
      warning('It has not much sense to looks for ngrams of an already tokened text, look carefully the results.')
    }
  )

  ngram.list(corpus = corpus, n_min = n_min, n_max = n_max,
    parallel = parallel, ..., ncores = ncores)
}

#' @rdname ngram
#' @export
ngram.default <- function(corpus, n_min = 1, n_max = 2, ...,
  parallel  = FALSE, ncores = parallel::detectCores() - 1
) {
  stop('corpus must be a list, a character vector or a VCorpus')
}

#' Bigram computation
#'
#' a shortcuts for \code{ngram} using \code{n_min = n_max = 2}
#'
#' @rdname ngram
#' @return (list) of character vectors containing the nGrammed documents
#' @export
#'
bigram  <- function(corpus, ..., parallel = FALSE,
  ncores = parallel::detectCores() - 1
) {
    ngram(
      corpus = corpus, n_min = 2, n_max = 2, ..., parallel = parallel,
      ncores = ncores
    )
}


#' Trigram computation
#'
#' a shortcuts for \code{ngram} using \code{n_min = n_max = 3}
#'
#' @rdname ngram
#' @return (list) of character vectors containing the nGrammed documents
#' @export
#'
trigram <- function(corpus, ..., parallel = FALSE,
  ncores = parallel::detectCores() - 1
) {
    ngram(
      corpus = corpus, n_min = 3, n_max = 3, ..., parallel = parallel,
      ncores = ncores
    )
}
