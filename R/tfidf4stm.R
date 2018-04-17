#' Tf-IDf for DTM
#'
#'
#' @param dtm an object of class \code{\link[tm]{DocumentTermMatrix}}
#'            rappresenting a DTM, in particular, with documents as row.
#' @param normalize (lgl) if \code{TRUE} (default) the weights are normalized
#'                  wrt documents, i.e. row_sum equals 1 for every row.
#' @param fun The weighting function for the ratio of document-frequencies.
#' @param adjust If \code{TRUE} (default) adds 1 to the denominator (i.e., the
#'               sum of the frequencies of a term into the corpora) to avoid
#'               the risk it happen to be zero.
#' @param force (lgl) if \code{TRUE} (default is \code{FALSE}) the function
#'              works also for \code{\link[slam]{simple_triplet_matrix}}
#'              which are not \code{\link[tm]{DocumentTermMatrix}}
#'
#' @return an object of class \code{\link[tm]{DocumentTermMatrix}} and
#'         \code{\link[slam]{simple_triplet_matrix}} (in the order).
#' @export
#'
tfidf4dtm <- function(dtm, normalize = TRUE, fun = log2, adjust = TRUE,
                      force = FALSE){

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

    att1 <- c('term frequency - inverse document frequency')

    if (normalize) {
      dtm  <- tf_normalizator(dtm, force = force)
      att1 <- 'term frequency - inverse document frequency (normalized)'
    }

    ## Tf-Idf
    #
    dtm <- slam:::t.simple_triplet_matrix(
      slam:::t.simple_triplet_matrix(dtm) *
        idf(dtm, fun = fun, adjust = adjust, force = force)
    )

    ## Define attributes
    #
    class(dtm) <- c('DocumentTermMatrix', 'simple_triplet_matrix')
    attr(dtm, "weighting") <- c(att1, 'tf-idf')

    dtm
}
