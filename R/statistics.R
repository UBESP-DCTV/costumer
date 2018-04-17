#' Calculation of different performance measures and statistical values
#'
#' \code{statistics} returns the different performance measures and statical values using the function \code{\link[caret]{confusionMatrix}} and
#'                   the package \code{pROC} for the AUC-ROC calculation
#'
#' @param pred factor containing the predicted classes
#' @param truth factor containing the reference classes
#' @param positive the first level of the factor or the level considered as positive class,
#'                 if factors are classified as positive and negative classes
#' @param dnn dimension names for the table
#' @param prevalence prevalence should be a single numeric value since we are using the binary factors
#' @param mode specifies either particular statistical values or everything returns all statistical values
#' @return list containing two elements. First element is a list with elements: table (confusion matrix), positive (the level of positive class),
#'         overall (overall accuracy and other statistic values) and byClass (the values of different performance measures as specified by the argument
#'         mode). Second element is the value: Area under the curve
#' @export
#'
#' @examples
#' library(hutch.code)
#' library(tm)
#' library(plyr)
#' truth <- factor(meta(liu_corpus)$real_label)
#' sam <- meta(liu_corpus)$real_label
#' pred <- factor(sample(sam))
#' statistics(pred, truth, positive = "1",  dnn = c("Prediction", "Truth"), prevalence = NULL, mode = "everything")
statistics <- function(pred, truth, positive, dnn, prevalence, mode){

  if(!is.factor(pred) || !is.factor(truth)){

    stop("pred and truth has to be factors")
  }
## statistics using Caret package
  stats <- caret::confusionMatrix(pred, truth,
    positive   = positive,
    dnn        = dnn,
    prevalence = prevalence,
    mode       = "everything"
  )

  if(!all.equal(levels(pred), c('0', '1'))){
    ## renaming the levels to '0', '1' so that it can be easily converted
    ## to numeric vector with same labels otherwise NAs will be coerced
  pred <- plyr::mapvalues(pred, from = levels(pred), to = c("0", "1"))
  }

  ## another way as.numeric(as.character(truth))
  pred <- as.numeric(levels(pred))[pred]

  ## AUC the package pROC
  roc_curve <- pROC::roc(response = truth, predictor = pred, plot = TRUE)
  auc_value <- pROC::auc(roc_curve)

  return(list(stats, auc_value))

}
