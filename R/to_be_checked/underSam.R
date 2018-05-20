#' Balancing of DTM
#'
#' @param  X      represents a DTM
#' @param  Y    the response variable of the unbalanced dataset
#' @param  perc     percentage of sampling depending on the method argument
#' @param  method   method to apply percentage of undersampling according to the majority class("percUnder") or minority class("percPos")
#' @param  w        undersampling with weighting of majotity class, if NULL sampling is done by giving qual weights
#'
#'
#'
#'
#' @return          value will be a list of 3 elements, the first element X will be the balanced STM of the same class
#'                  as the input stm i.e c('DocumentTermMatrix', 'simple_triplet_matrix'), the second element Y will contain the
#'                  response variable of the balanced data as factors and the third element will contain integers
#'                  representing the removed documents.
#' @export
#' @examples
#' class <- meta(liu_corpus)$real_label
#' stm <- liu_dtm
#' test <- rander_sampling(stm, class, perc = 50, method = "percUnder", w = NULL)
#' test$X
#' test$Y
#' test$id.rm
rander_sampling <- function(X, Y, type = "ROS", perc = 50, k = 0, w = NULL, verbose = FALSE){

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

    modfdata <- ubUnder(X = X, Y = Y, perc, method = "percPos", w)
    return(modfdata)

  }
  if (type == "RUS_under"){ #Random Undersampling

    modfdata <- ubUnder(X = X, Y = Y, perc, method = "percUnder", w)
    return(modfdata)


  }
  if (type == "ROS"){ #Random Over Sampling

    modfdata <- ubOver(X, Y, k, verbose)
     return(modfdata)
  }

  if (type_tech == "SMOTE"){
    stop("still needs to be completed")
  }


}


