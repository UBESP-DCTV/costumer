#' Dense matrix to Simple Triplet-Matrix
#'
#' @param dns a full dense \code{matrix} with rownames as documents and
#'            colnames as tokens
#' @param memory (num) the maximum ammount of memory (RAM + virtual) the sistem
#'  is required to reserve for the proces. Default is to use standard R option
#'  and so provide "memory.limit()"
#'
#' @return an object of classes
#'         \code{link[tm]{DocumentTermMatrix}} and
#'         \code{link[slma]{simple_triplet_matrix}}
#' @export
#'
#' @examples
#' library(slam)
#' data("liu_dtm")
#' liu_full_matrix <- stm2dns(liu_dtm)
#' identical(liu_dtm, slam::as.simple_triplet_matrix(liu_full_matrix))
dns2stm <- function(dns, memory = NULL){

  if (!inherits(dns, 'weighted')) {
    stop('this function is create only for matrices of class weighted')
  }


  if (!is.null(memory)) {
    if (utils::memory.limit() < memory) {
      message('You are going to increase the maximum ammount of RAM that R can use.')
    }
    utils::memory.limit(memory) #200000
  }

  stm <- slam:::as.simple_triplet_matrix.matrix(dns)

  ord_i <- order(stm[['i']], stm[['j']])
  stm[['i']] <- stm[['i']][ord_i]
  stm[['j']] <- stm[['j']][ord_i]
  stm[['v']] <- stm[['v']][ord_i]

  names(stm[['dimnames']]) <- c('Docs', 'Terms')

  class(stm) <- c('DocumentTermMatrix', 'simple_triplet_matrix')
  attr(stm, "weighting") <- attr(dns, "weighting")
  stm
}


#' @rdname dns2stm
#'
#' @param df a data frame to be converted in STM
#'
#' @param force (lgl) flag (default = FALSE) to force the use also for
#'              non-weihted data frames
#'
#'
#' @export
as.simple_triplet_matrix.data.frame <- function(df, memory = NULL,
  force = FALSE
){
  if (!force) {

    if (!inherits(df, 'weighted')) {
      stop('this function is create only for data frames of class weighted')
    }
  }

  if (!is.null(memory)) {
    if (utils::memory.limit() < memory) {
      message('You are going to increase the maximum ammount of RAM that R can use.')
    }
    utils::memory.limit(memory) #200000
  }

  stm <- slam:::as.simple_triplet_matrix.matrix(as.matrix.data.frame(df))

  ord_i <- order(stm[['i']], stm[['j']])
  stm[['i']] <- stm[['i']][ord_i]
  stm[['j']] <- stm[['j']][ord_i]
  stm[['v']] <- stm[['v']][ord_i]

  names(stm[['dimnames']]) <- c('Docs', 'Terms')

  class(stm) <- c('DocumentTermMatrix', 'simple_triplet_matrix')
  attr(stm, "weighting") <- attr(df, "weighting")
  stm
}

