#' Term Frequencies Normalizator for Document-Term Matrices
#'
#' The aims of this function is to normalize the term frequencies in a
#' document-term frequencies, i.e. each frequencies of each documents in the
#' matrix is divided by the sum of all the frequencies of the terms in the
#' document.
#' a
#'
#' @param dtm A \code{\link[tm]{DocumentTermMatrix}} filled with the simple
#'            term-frequencies weights
#' @param force (lgl) if \code{TRUE} (default is \code{FALSE}) the function
#'              works also for \code{\link[slam]{simple_triplet_matrix}}
#'              which are not \code{\link[tm]{DocumentTermMatrix}}
#'
#' @return A \code{\link[tm]{DocumentTermMatrix}} for wich the sum of the
#'         frequencies of each document is 1 (or 0 if it is an empty one)
#' @export
#'
#' @examples
#' data(liu_dtm)
#' tf_normalizator(liu_dtm)
tf_normalizator <- function(dtm, force = FALSE) {

  if (!force) {
    if (!inherits(dtm, 'DocumentTermMatrix')) {
      stop('dtm has to be a DocumentTermMatrix object.')
    }

    if (any(attr(dtm, "weighting") != c('term frequency', 'tf'))) {
      stop('weight for the source dtm has to be the simple term-frequencies')
    }
  } else {
    if (!inherits(dtm, 'simple_triplet_matrix')) {
      stop('dtm has to be at least a simple_triplet_matrix.')
    }
  }


  ds <- as.integer(slam::row_sums(dtm))

  if (all(ds %in% c(0, 1))) return(dtm)

  dtm[['v']] <- dtm[['v']] / ds[dtm[['i']]]

  attr(dtm, 'weighting')[[1]] <- 'term frequency (normalized)'

  dtm
}



#' Inverse document-frequencies Calculator
#'
#' The aims of this function is to compute the Inverse document-frequencies
#' possibly using different functions. The standard one is the \code{log2}
#'
#' @param dtm A \code{\link[tm]{DocumentTermMatrix}} filled with the simple
#'            term-frequencies weights
#'
#' @param fun The weighting function for the ratio of document-frequencies
#' @param adjust If \code{TRUE} (default) adds 1 to the denominator (i.e., the
#'               sum of the frequencies of a term into the corpora) to avoid
#'               the risk it happen to be zero.
#' @param force (lgl) if \code{TRUE} (default is \code{FALSE}) the function
#'              works also for \code{\link[slam]{simple_triplet_matrix}}
#'              which are not \code{\link[tm]{DocumentTermMatrix}}
#'
#' @return A numerical vector representing the inverse document-frequencies
#' @export
#'
#' @examples
#' data(liu_dtm)
#' idf(liu_dtm)
#' idf(liu_dtm, adjust = FALSE)
#' idf(liu_dtm, fun = log)
idf <- function(dtm, fun = log2, adjust = TRUE, force = FALSE) {

  if (!force) {
    if (!inherits(dtm, 'DocumentTermMatrix')) {
      stop('dtm has to be a DocumentTermMatrix object.')
    }

    if ((attr(dtm, "weighting")[[1]] != 'term frequency' &&
         attr(dtm, "weighting")[[1]] != 'term frequency (normalized)'
         ) ||
        attr(dtm, "weighting")[[2]] != 'tf'
    ) {
      stop('weight for the source dtm has to be the simple term-frequencies')
    }
  } else if (!inherits(dtm, 'simple_triplet_matrix')) {
      stop('dtm has to be at least a simple_triplet_matrix.')
  }

  if (!is.function(fun)) stop('fun has to be function')

  ts <- slam::col_sums(dtm != 0) %>%
    as.integer

  if (adjust){
    ts <- ts + 1
  }

  tryCatch(
    purrr::map_dbl(ts, ~ fun(dtm[['nrow']] / .)),
    error = function(e) {
      stop(
        c('fun has to admit numerical input and return numerical output.\n',
             'Original error: ', e
        )
      )
    }
  )
}
