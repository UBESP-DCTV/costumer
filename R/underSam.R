#' Balancing of DTM
#'
#' \code{hutch_sampling} returns the balanced document term matrix using Randon Undersampling and Random Oversampling techniques
#'
#'  This function applies balancing techniques: Random Undersampling and Random Oversampling on the document term matrix
#'  using the functions \code{\link[unbalanced]{ubUnder}} and \code{\link[unbalanced]{ubOver}}.
#'
#' @param  X        represents a DTM with the class c('DocumentTermMatrix', 'simple_triplet_matrix')
#' @param  Y        the response variable of the unbalanced dataset, should be a factor with two levels (binary)
#' @param  type     technique for balancing i.e. either Random Oversampling (ROS) or
#'                  two different ways of applying Random Undersampling  (RUS) i.e "RUS_under" type to
#'                  apply percentage of undersampling according to the majority class("percUnder") or RUS_Pos minority class("percPos")
#' @param  perc     argument for the type "RUS_under" and "RUS_Pos" only i.e. percentage of sampling of majority class depending on the type of
#'                  RUS i.e (RUS_under, RUS_Pos)
#' @param  k_pos    argument for the type "ROS" only, number of times of positve (minority) instances to be generated
#' @param  w        argument for the type "RUS_under" and "RUS_Pos only", undersampling with weighting of majotity class, if NULL sampling is done
#'                  by giving qual weights
#' @param  verbose  argument only for the type = "ROS" only. If TRUE, prints extra information
#'
#'
#'
#'
#' @return          if type = "RUS_Pos" or "RUS_under", value will be a list of 3 elements. The first element X will be the balanced DTM of the same class
#'                  as the input DTM i.e c('DocumentTermMatrix', 'simple_triplet_matrix'), the second element Y will contain the
#'                  response variable of the balanced data as factors and the third element will contain a vector representing the removed documents.
#'
#'                  if type = "ROS", value will be a list of two elements. The first element X will be the balanced DTM of the same class
#'                  as the input DTM i.e c('DocumentTermMatrix', 'simple_triplet_matrix'), the second element Y will contain the
#'                  response variable of the balanced data as factors
#'
#' @export
#' @examples
#' library(tm)
#' library(unbalanced)
#'
#' y <- factor(meta(liu_corpus)$real_label)
#' x <- liu_dtm
#'
#' exp <- hutch_sampling(x, y, type = "RUS_Pos", perc = 50, k_pos = 0,
#'   w = NULL, verbose = FALSE
#' )
#'
#' exp$X
#' exp$Y
#' exp$id.rm
#'
#'
#' test <- hutch_sampling(x, y, type = "ROS", perc = 50, k_pos = 2,
#'   w = NULL, verbose = FALSE
#' )
#'
#' test$X
#' test$Y
hutch_sampling <- function(X, Y, type = "ROS", perc = 50, k_pos = 0,
                           w = NULL, verbose = TRUE
){

  if(!inherits(X, c('DocumentTermMatrix', 'simple_triplet_matrix')))
    stop('X should be a Simple Triplet Matrix representing a DTM')

  if (any(is.na(Y))) {
    stop("Y has NAs")
  }

  # response variable should be a binary factor
  if( !is.factor(Y)) {
    stop("Y must be a factor")
  }

  if (length(levels(Y)) != 2){
    stop("Y must be a binary factor variable")
  }

  type = match.arg(type, c("RUS_Pos", "RUS_under", "ROS", "SMOTE"))
  if (type == "RUS_Pos"){ #Random Undersampling

    modfdata <- unbalanced::ubUnder(X = X, Y = Y, perc, method = "percPos", w)
    return(modfdata)

  }
  if (type == "RUS_under"){ #Random Undersampling

    modfdata <- unbalanced::ubUnder(X = X, Y = Y, perc, method = "percUnder", w)
    return(modfdata)


  }
  if (type == "ROS"){ #Random Over Sampling

    X <- as.data.frame.simple_triplet_matrix(X)
    modfdata <- unbalanced::ubOver(X, Y, k = k_pos, verbose)
    X <- as.simple_triplet_matrix.data.frame(modfdata$X)
    return(list(X = X, Y = modfdata$Y))
  }

}



