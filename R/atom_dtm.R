#' Create a dtm from a corpus (tf weights)
#'
#' \code{atom_dtm} take a corpora, tokenized or not, and create the
#' corresponding \code{\link[tm]{DocumentTermMatrix}} (DTM) stored as sparse
#' \code{\link[slam]{simple_triplet_matrix}} (see Details).
#'
#' The algrithm of the simple triplet matrix considers three indeces \code{i},
#' \code{j}, \code{v}, in which the indeces \code{i}, \code{j} represent
#' respectively the row (document) and the column (term/token) coordinate of an
#' entry \code{v} rapresent its weight (commonly the frequency).
#'
#' Moreover, for compatibility reasons (with some machine learning R
#' implementation of algorithms which use different convention for the
#' representation of sparse matrices), the indeces are ordered with priority
#' \code{i}, \code{j}.
#'
#' @param corpus (list) of documents, or a list of character vectors, each
#'               element reporting tokens from a document
#'
#' @param step (num) integer value (default is 500L) used to broken the
#'             procedure in parts of at maximum \code{step} documents each one.
#'             This is to help to don't overflow the RAM.
#'
#' @param parallel (lgl) if \code{TRUE} (default is \code{FALSE}) run parallel
#'                 computations using \code{\link[parallel]{makePSOCKcluster}}
#'                 backend with max - 1 core.
#'
#' @param ... further option passed to the function
#'
#' @param ncores (int) number of core to use in the parallel computation
#'        (default is number of machine cores minus one)
#'
#' @return a multiclass \code{\link[tm]{DocumentTermMatrix}} and
#'         \code{\link[slam]{simple_triplet_matrix}} object weigthed with simple
#'         term frequencies, rappresenting a document-term matrix in which each
#'         row represent a document, each columns a term (or token) and the
#'         content the simple frequencies of the terms in the document.
#'
#' @export
#'
#' @examples
#' data(liu_4h28)
#' corpus <- data2corpus(liu_4h28)
#' atom_dtm(corpus)
#' atom_dtm(c('one', 'two', 'one two'))             # three documents, two token
#' atom_dtm(c('one', 'two', 'one two'), docs_or_tokens = 'tokens')    # one docs
#'
#' \dontrun{
#'   atom_dtm(corpus, parallel = TRUE)                    # parallel computation
#'   atom_dtm(c(1, 2, 3))                                 # error
#' }
atom_dtm <- function(corpus, step = 500L, parallel = FALSE, ...,
    ncores = parallel::detectCores() - 1
) {
  UseMethod('atom_dtm')
}

#' @rdname atom_dtm
#' @export
atom_dtm.list <- function(corpus, step = 500L, parallel = FALSE, ...,
    ncores = parallel::detectCores() - 1
) {

  pb <- progress::progress_bar$new(
    format = " ct_corpus_and_dtm [:bar] :percent in :elapsed",
    total  = 14,
    clear  = FALSE,
    width  = 60
  )

  pb$tick(0, token = list(what = 'start'))

  ## Check if the corpus is a list of tokenized document or a list of
  ## full documents, in the latter case tokenize them at word level
  #
  pb$tick(token = list(what = 'tokenization'))
  if(!check_tokened(corpus)){
    corpus <- purrr::map(corpus, ~ unlist(stringi::stri_extract_all_words(.)))
  }

  ## The features are the single occurences of each token, ordered for a
  ## better exploratory analyses
  #
  pb$tick(token = list(what = 'features'))
  features <- sort(unique(unlist(corpus)))

  ## we want a matrix made by documents in rows and token in columns. For
  ## Memory needed all the procedure are broken in steps of "step" length
  ## (user defined and 500 by default)
  #
  n.row <- length(corpus)
  n.col <- length(features)

  n.step <- n.row %/% step + (n.row %% step != 0)
  start  <- seq.int(1, n.row, step)
  end    <- unlist(purrr::map(start + (step - 1), min, n.row))

  ## Create an empty simple triplet (sparse) matrix
  #
  DT.triple <- slam::simple_triplet_zero_matrix(
    nrow = 0,
    ncol = n.col,
    mode = 'integer'
  )

  ## sequential creation of DTM
  #
  pb$tick(token = list(what = 'dtm content'))
  if (parallel){
    message('Computation run parallelized using PSOCK clusters')
    cl <- parallel::makePSOCKcluster(ncores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
  }
  for (i in seq_len(n.step)) {

    message(paste0('progress: ', 100 * (i-1) / n.step, '%'))

    ## computate and save the content of the Document Term matrix for
    ## each of the steps, all to be merge together in the next step
    #
    now_length <- length(start[i]:end[i])

    if (!exists('DTcontent') || length(DTcontent) != now_length) {
      DTcontent <- vector('list', now_length)
    }

    ## set and activate the cluster
    #
    if (parallel) {
      #
      DTcontent[] <- parallel::parLapply(
        cl  = cl,
        X   = corpus[start[i]:end[i]],
        fun = function(tokens_doc, features){
          tabulate(
            factor(tokens_doc,
                   levels  = features,
                   ordered = TRUE
            ),
            nbins = length(features)
          )
        },
        features = features
      )
    } else { # if parallel = FALSE
      DTcontent[] <- purrr::map(corpus[start[i]:end[i]],
        ~ tabulate(
            factor(., levels  = features, ordered = TRUE),
            nbins = length(features)
          )
      )
    }

    #
    DT.triple <- slam:::rbind.simple_triplet_matrix(
      DT.triple,
      slam::as.simple_triplet_matrix(
        do.call(rbind, DTcontent)
      )
    )

    if (i == n.step) message('progress: 100%')
  } # END OF THE FOR CICLE

  ## fixes order to avoid issues with function
  ## maxent::as.compressed.matrix which required
  ## a scr standard (ordered first by row and next by colums)
  #
  pb$tick(token = list(what = 'ordering'))
  ord_i <- order(DT.triple[['i']], DT.triple[['j']])
  DT.triple[['i']] <- DT.triple[['i']][ord_i]
  DT.triple[['j']] <- DT.triple[['j']][ord_i]
  DT.triple[['v']] <- DT.triple[['v']][ord_i]
  #

  #
  pb$tick(token = list(what = 'attributes'))
  rownames(DT.triple) <- names(corpus)
  colnames(DT.triple) <- features                               # DTM !!!!

  names(DT.triple$dimnames) <- c('Docs', 'Terms')

  class(DT.triple) <- c('DocumentTermMatrix', 'simple_triplet_matrix')
  attr(DT.triple, "weighting") <- c('term frequency', 'tf')

  #
  pb$tick(token = list(what = 'End'))
  DT.triple
  #
}


#' @rdname atom_dtm
#' @export
atom_dtm.VCorpus <- function(corpus, step = 500L, parallel = FALSE, ...,
  ncores = parallel::detectCores() - 1
) {

  ## Convert the corpus to a List
  #
  corpus <- corpora2list(corpus)
  atom_dtm.list(corpus, step = step, parallel = parallel, ..., ncores = ncores)
}


#' @rdname atom_dtm
#' @param docs_or_tokens (chr) if \code{docs} (default) means that the
#'          sequencies of elements of the character vector represent a document
#'          each one, if \code{tokens} means that they represents the sequencies
#'          of tokens of one single documents
#' @export
atom_dtm.character <- function(corpus, step = 500L, parallel = FALSE, ...,
  ncores = parallel::detectCores() - 1, docs_or_tokens = c('docs', 'tokens')
) {

  docs_or_tokens = match.arg(docs_or_tokens)

  ## If the vector represents a list of documents convert to it, if represents
  ## a list of tokens of a single documents convert it to a single-element
  ## list
  #
  switch(docs_or_tokens,
    docs   = {corpus <- as.list(corpus)},
    tokens = {corpus <- list(corpus)}
  )

  atom_dtm.list(corpus, step = step, parallel = parallel, ..., ncores = ncores)
}

#' @rdname atom_dtm
#' @export
atom_dtm.default <- function(corpus, step = 500L, parallel = FALSE, ...,
  ncores = parallel::detectCores() - 1
) {
  stop('corpus must be a list, a character vector or a VCorpus')
}

