#' Left filter for DTMs
#'
#' Left filter for document-term matrix structure (as provided by \pkg{tm}).
#' I.e. return a \code{\link[tm]{DocumentTermMatrix}} for the \code{target} dtm
#' documents considering all (and only) the terms in the \code{reference} dtm, filling
#' with zeros the terms which are not present. Final order of token are the
#' same as in \code{target}
#'
#' This is usefull in Machine Learning, i.e. to use a machine \code{reference}-trained
#' on the \code{target} dtm.
#'
#' @param target (\code{\link[tm]{DocumentTermMatrix}}) This is the dtm considered
#'   for the list of documents, i.e. the returned dtm will have the exact
#'   documents (with the same order) of this one.
#' @param reference (\code{\link[tm]{DocumentTermMatrix}}) This is the dtm considered
#'   for the list of terms, i.e. the returned dtm will have the exact term (with
#'   the same order) of this one.
#'
#' @return (\code{\link[tm]{DocumentTermMatrix}}) with weights for the terms in
#'         \code{reference} and documents in \code{target} DTMs.
#' @export
#'
#' @seealso \link{tm}, \link{slam}
#' @examples
#' library(tm)
#' data(acq)
#' data(crude)
#'
#' reference <- DocumentTermMatrix(acq)
#' to_adjust <- DocumentTermMatrix(crude)
#'
#' dtm_lfilter(reference, to_adjust)
dtm_lfilter <- function(target, reference){

  if (!inherits(reference, 'DocumentTermMatrix')) {
      stop('reference has to inherits to DocumentTermMatrix or TermDocumentMatrix classes')
  }

  if (!inherits(target, 'DocumentTermMatrix')) {
      stop('target has to inherits to DocumentTermMatrix or TermDocumentMatrix classes')
  }

  ## Retrieve the index of the terms in `reference` that are not in `target` (which will be
  ## considered even if they are not present in the `target` documents); and the ones
  ## that are in `target` but not in `reference` (which will not be considered even if they
  ## are actualy terms of documents in `reference`).
  #
  ## note: use of which is mandatory because at the moment `slam` do not manage
  ##       lgl subsetting for stm
  #
  idx     <- !reference$dimnames$Terms %in% target$dimnames$Terms
  idx_neg <- which(target$dimnames$Terms %in% reference$dimnames$Terms)
  if (length(idx_neg) == 0L) {
    idx_neg <- 0
  }
  ## The content of the resulting DTM will be a subset of the `target` one, so
  ## initialize it.
  #
  res <- target

  ## combine the Terms in `target` which are also Terms of `reference` with the Terms of
  ## `reference` which are not in `target`.
  ##
  ## Update the number of Terms (i.e. the `ncol` field of the DTM)
  #
  res <- res[, idx_neg]                    # drop = FALSE is the default for stm
  res$dimnames$Terms <- c(res$dimnames$Terms, reference$dimnames$Terms[idx])
  res$ncol <- res$ncol + length(idx)

  ## Reorder the Terms in the same way as in `reference`
  #
  ord <- order(res$dimnames$Terms)[order(order(reference$dimnames$Terms))]
  res[, ord]
}
