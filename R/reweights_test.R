#' Weights for Test dtm
#'
#' The aims of this function is to provide to a test Document-Term Matrix which
#' will be used for test/predict the original weights computed for the training
#' matrix.
#'
#' @param target A \code{\link[tm]{DocumentTermMatrix}} to weigtht
#' @param original The original \code{\link[tm]{DocumentTermMatrix}} with the
#'                 simple term-frequencies weights
#' @param normalize (lgl) if \code{TRUE} (default) the weights are normalized
#'                  wrt documents, i.e. row_sum equals 1 for every row.
#' @param ...  Other parameters to called functions (e.g. \code{fun})
#' @param fun The \code{function} used to compute the original weigths. By
#'            default \code{\link{idf}}. (Suggestion is to not
#'            change this option!). It has to admit as first argument the
#'            orignial \code{\link[tm]{DocumentTermMatrix}}.
#' @param original_weights optional numerical vector of weights to be used
#'        directly for the rewaighting of target
#' @param force (lgl) if \code{TRUE} (default is \code{FALSE}) the function
#'              works also for \code{\link[slam]{simple_triplet_matrix}}
#'              which are not \code{\link[tm]{DocumentTermMatrix}}
#'
#' @return an object of class \code{\link[tm]{DocumentTermMatrix}} and
#'         \code{\link[slam]{simple_triplet_matrix}} (in the order).
#' @export
#'
reweights_test <- function(target, original, normalize = TRUE, ...,
    fun = idf, original_weights = NULL, force = FALSE
) {
  if (!force) {
    if (!inherits(target, 'DocumentTermMatrix') ||
        !inherits(original, 'DocumentTermMatrix')) {
          stop('target and original has to be a DocumentTermMatrix objects.')
    }

    if (any(attr(target, "weighting") != c('term frequency', 'tf'))) {
      stop('weight for the target dtm has to be the simple term-frequencies')
    }

    if (any(attr(original, "weighting") != c('term frequency', 'tf'))) {
      stop('weight for the original dtm has to be the simple term-frequencies')
    }
  } else  {
    if (!inherits(target, 'simple_triplet_matrix')) {
      stop('target has to be at least a simple_triplet_matrix.')
    }
    if (!inherits(original, 'simple_triplet_matrix')) {
      stop('original has to be at least a simple_triplet_matrix.')
    }
  }

  if (!is.function(fun)) stop('fun has to be function')


  null_weights <- is.null(original_weights)

  if (!null_weights && (length(original_weights) != dim(target)[[2]])) {
    stop('orignial_weights has to be a vector of length equal to the number of token of target')
  }
  if (null_weights && length(target) != length(original)){
    stop('number of tokens in target has to be the same as in original')
  }

  if (normalize) {
    target  <- tf_normalizator(target, force = force)
    attr_wgts <- c(
      'term frequency - inverse document frequency (normalized)', 'tf-idf'
    )
  } else {
    attr_wgts <- c('term frequency - inverse document frequency', 'tf-idf')
  }

  if (null_weights) {
    if (!identical(fun, idf)) {
      warning('"fun" not standard, attribute "weighting" for target could be wrongly assigned.')
    }
    if ('force' %in% methods::formalArgs(args(fun))) {
      wgts <- fun(original, ..., force = force)
    } else {
      wgts <- fun(original, ...)
    }
  } else {
    wgts <- original_weights
  }


  ## Weigths
  #
  target <- slam:::t.simple_triplet_matrix(
    slam:::t.simple_triplet_matrix(target) * wgts
  )

  ## Define attributes
  #
  class(target) <- c('DocumentTermMatrix', 'simple_triplet_matrix')
  attr(target, "weighting") <- attr_wgts

  target
}
