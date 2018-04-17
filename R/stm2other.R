#' Simple Triplet-Matrix to Dense matrix
#'
#' @param dtm an object of simple_triplet_matrix class
#' @param memory (num) the maximum ammount of memory (RAM + virtual) the sistem
#'  is required to reserve for the proces. Default is to use standard R option
#'  and so provide "memory.limit()"
#'
#' @return \code{\link[hutch.code]{stm2dns}} returns a full dense \code{matrix}
#' @export
#'
#' @examples
#' data("liu_dtm")
#' liu_full_matrix <- stm2dns(liu_dtm)
#' liu_df <- as.data.frame(liu_dtm)
stm2dns <- function(dtm, memory = NULL){

    if (!is.null(memory)) {
      if (memory.limit() < memory) {
        message('You are going to increase the maximum ammount of RAM that R can use.')
      }
      memory.limit(memory) #200000
    }

    y <- matrix(as.integer(0), dtm[['nrow']], dtm[['ncol']])

    n <- list(1L)
    while (n[[1L]] <= length(dtm[['v']])) {
      y[[dtm[['i']][[n[[1L]]]], dtm[['j']][[n[[1L]]]]]] <- dtm[['v']][[n[[1L]]]]
      n[[1L]] <- n[[1L]] + 1L
    }

    rownames(y) <- dtm[['dimnames']][['Docs']]
    colnames(y) <- dtm[['dimnames']][['Terms']]

    if (!is.null(attr(dtm, 'weighting'))) {
      attr(y, 'weighting') <- attr(dtm, 'weighting')
      class(y) <- c(class(y), 'weighted')
    }
    y
}


#' @rdname stm2dns
#' @return \code{\link[hutch.code]{as.data.frame.simple_triplet_matrix}}
#'          returns a \code{data.frame}
#' @export
as.data.frame.simple_triplet_matrix <- function(dtm, memory = NULL){
  y <- as.data.frame.matrix(stm2dns(dtm = dtm, memory = memory))
  if (!is.null(attr(dtm, 'weighting'))) {
    attr(y, 'weighting') <- attr(dtm, 'weighting')
    class(y) <- c(class(y), 'weighted')
  }
  y
}
