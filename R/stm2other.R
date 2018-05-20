#' Simple Triplet-Matrix to Dense matrix
#'
#' @param x an object of simple_triplet_matrix class
#' @param memory (num) the maximum ammount of memory (RAM + virtual) the sistem
#'  is required to reserve for the proces. Default is to use standard R option
#'  and so provide "memory.limit()"
#'
#' @return \code{\link{stm2dns}} returns a full dense \code{matrix}
#' @export
#'
#' @examples
#' data("liu_dtm")
#' liu_full_matrix <- stm2dns(liu_dtm)
#' liu_df <- as.data.frame(liu_dtm)
stm2dns <- function(x, memory = NULL){

    if (!is.null(memory)) {
      if (utils::memory.limit() < memory) {
        message('You are going to increase the maximum ammount of RAM that R can use.')
      }
      utils::memory.limit(memory) #200000
    }

    y <- matrix(as.integer(0), x[['nrow']], x[['ncol']])

    n <- list(1L)
    while (n[[1L]] <= length(x[['v']])) {
      y[[x[['i']][[n[[1L]]]], x[['j']][[n[[1L]]]]]] <- x[['v']][[n[[1L]]]]
      n[[1L]] <- n[[1L]] + 1L
    }

    rownames(y) <- x[['dimnames']][['Docs']]
    colnames(y) <- x[['dimnames']][['Terms']]

    if (!is.null(attr(x, 'weighting'))) {
      attr(y, 'weighting') <- attr(x, 'weighting')
      class(y) <- c(class(y), 'weighted')
    }
    y
}


#' @rdname stm2dns
#'
#' @param ... further arguments passed to the function
#'
#' @return \code{\link{as.data.frame.simple_triplet_matrix}}
#'          returns a \code{data.frame}
#' @export
as.data.frame.simple_triplet_matrix <- function(x, ...,  memory = NULL){
  y <- as.data.frame.matrix(stm2dns(x = x, memory = memory))
  if (!is.null(attr(x, 'weighting'))) {
    attr(y, 'weighting') <- attr(x, 'weighting')
    class(y) <- c(class(y), 'weighted')
  }
  y
}
