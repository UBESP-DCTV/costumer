# ==========================================================================
# ========================  svmLinear2_cvAble ==============================
# ==========================================================================

svmLinear2_cvAble <- caret:::getModelInfo()$svmLinear2

# label --------------------------------------------------------------------
#
svmLinear2_cvAble$label <- stringr::str_c(
  caret:::getModelInfo()$svmLinear2$label,
  '(with possibilities to manage data in the inner resampling steps)'
)

# libraries ----------------------------------------------------------------
#
svmLinear2_cvAble$library <- c(caret:::getModelInfo()$svmLinear2$library,
                               'hutch.code'
)

# fit ----------------------------------------------------------------------
#
svmLinear2_cvAble$fit <- function(
  x, y, wts, param, lev, last, classProbs, ...) {

  requireNamespace('hutch.code')
#  message(paste('fit original x', str(x)))
  if (is.data.frame(x)) {
    x <- hutch.code:::as.simple_triplet_matrix.data.frame(x, force = TRUE)
  }

  xx <- hutch.code::tfidf4dtm(dtm = x, force = TRUE)
# message(paste('fit used x', str(xx)))

  if (any(names(list(...)) == "probability") | is.numeric(y)) {
    out <- svm(x = hutch.code:::stm2dns(xx, memory.limit()), y = y, kernel = "linear",
               cost = param$cost, scale = FALSE, ...)
  }
  else {
    out <- svm(x = hutch.code:::stm2dns(xx, memory.limit()), y = y, kernel = "linear",
               cost = param$cost, probability = classProbs, scale = FALSE, ...)
  }
# message(paste('fit out ', str(out)))
  attr(out, 'original_train') <- x
  attr(out, 'used_train')     <- xx
  out
}

# predict ----------------------------------------------------------------
#
svmLinear2_cvAble$predict <- function(
  modelFit, newdata, submodels = NULL, ...
) {

#  message(paste('predict input ', str(newdata)))
#  message(paste('predict model ', str(modelFit)))

  requireNamespace('hutch.code')
  requireNamespace('slam')
  if (inherits(newdata, 'data.frame')) {
    newdata <- hutch.code:::as.simple_triplet_matrix.data.frame(
      newdata,
      force = TRUE
    )
  }
#  message(paste('predict to be weighted ', str(newdata)))

  xx <- hutch.code::reweights_test(
    target   = newdata,
    original = attr(modelFit, 'original_train'),
    force   = TRUE
  )
#  message(paste('predict used ', str(xx)))

  out <- e1071:::predict.svm(modelFit, hutch.code:::stm2dns(xx, memory.limit()))
#  message(paste('predict output ', str(out)))

  out
}


# prob -------------------------------------------------------------------
#
svmLinear2_cvAble$prob <- function(
  modelFit, newdata, submodels = NULL, ...
) {

  requireNamespace('hutch.code')
  requireNamespace('slam')
  if (inherits(newdata, 'data.frame')) {
    newdata <- hutch.code:::as.simple_triplet_matrix.data.frame(
      newdata,
      force = TRUE
    )
  }

  xx <- hutch.code::reweights_test(
    target   = newdata,
    original = attr(modelFit, 'original_train'),
    force   = TRUE
  )

  out <- e1071:::predict.svm(modelFit, hutch.code:::stm2dns(xx, memory.limit()), probability = TRUE)
  attr(out, "probabilities")
  out
}



# predictors -------------------------------------------------------------------
#
svmLinear2_cvAble$predictors <- function (x, ...)
{
  message(paste('predictors input ', str(x)))
  out <- if (!is.null(x$terms))
    predictors.terms(x$terms)
  else x$xNames
  if (is.null(out))
    out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
  if (is.null(out))
    out <- NA
  message(paste('predictors output ', str(out)))
  out
}



# levels -------------------------------------------------------------------
#
svmLinear2_cvAble$levels <- function (x) {
#  message(paste('levels input ', str(x)))
  x$levels
}

# sort -------------------------------------------------------------------
#
svmLinear2_cvAble$sort <- function (x)
{
#  message(paste('sort input ', str(x)))
  x[order(x$cost), ]
#  message(paste('sort output ', str(x)))
}

# saving -------------------------------------------------------------------
#
devtools::use_data(svmLinear2_cvAble, overwrite = TRUE)


# ==========================================================================
# ============================  nb_cvAble ==================================
# ==========================================================================

nb_cvAble <- caret:::getModelInfo()$nb

# label --------------------------------------------------------------------
#
nb_cvAble$label <- stringr::str_c(
  caret:::getModelInfo()$nb$label,
  '(with possibilities to manage data in the inner resampling steps)'
)

# libraries ----------------------------------------------------------------
#
nb_cvAble$library <- c(caret:::getModelInfo()$nb$library, 'hutch.code')

# fit ----------------------------------------------------------------------
#
nb_cvAble$fit <- function(
  x, y, wts, param, lev, last, classProbs, ...) {

  requireNamespace('hutch.code')

  if (is.data.frame(x)) {
    x <- hutch.code:::as.simple_triplet_matrix.data.frame(x, force = TRUE)
  }

  xx <- hutch.code::tfidf4dtm(dtm = x, force = TRUE)

  if (param$usekernel) {
    out <- NaiveBayes(stm2dns(xx, memory.limit()), y, usekernel = TRUE, fL = param$fL,
             adjust = param$adjust, ...)
  }
  else out <- NaiveBayes(stm2dns(xx, memory.limit()), y, usekernel = FALSE, fL = param$fL,
                ...)

  attr(out, 'original_train') <- x
  attr(out, 'used_train')     <- xx
  out
}

# predict ----------------------------------------------------------------
#
nb_cvAble$predict <- function(
  modelFit, newdata, submodels = NULL, ...
) {

  requireNamespace('hutch.code')
  requireNamespace('slam')
  if (inherits(newdata, 'data.frame')) {
    newdata <- hutch.code:::as.simple_triplet_matrix.data.frame(
      newdata,
      force = TRUE
    )
  }

  xx <- hutch.code::reweights_test(
    target   = newdata,
    original = attr(modelFit, 'original_train'),
    force   = TRUE
  )

  xx <- hutch.code:::as.data.frame.simple_triplet_matrix(xx)
  predict(modelFit, xx)$class

}


# prob -------------------------------------------------------------------
#
nb_cvAble$prob <- function(
  modelFit, newdata, submodels = NULL, ...
) {
  requireNamespace('hutch.code')
  requireNamespace('slam')
  if (inherits(newdata, 'data.frame')) {
    newdata <- hutch.code:::as.simple_triplet_matrix.data.frame(
      newdata,
      force = TRUE
    )
  }

  xx <- hutch.code::reweights_test(
    target   = newdata,
    original = attr(modelFit, 'original_train'),
    force   = TRUE
  )

  xx <- hutch.code:::as.data.frame.simple_triplet_matrix(xx)
  predict(modelFit, xx, type = "raw")$posterior
}

# saving -------------------------------------------------------------------
#
devtools::use_data(nb_cvAble, overwrite = TRUE)




# ==========================================================================
# ============================  knn_cvAble =================================
# ==========================================================================

knn_cvAble <- caret:::getModelInfo()$knn

# label --------------------------------------------------------------------
#
knn_cvAble$label <- stringr::str_c(
  caret:::getModelInfo()$knn$label,
  '(with possibilities to manage data in the inner resampling steps)'
)

# libraries ----------------------------------------------------------------
#
knn_cvAble$library <- c(caret:::getModelInfo()$knn$library, 'hutch.code')

# fit ----------------------------------------------------------------------
#
knn_cvAble$fit <- function(
  x, y, wts, param, lev, last, classProbs, ...) {

  requireNamespace('hutch.code')

  if (is.data.frame(x)) {
    x <- hutch.code:::as.simple_triplet_matrix.data.frame(x, force = TRUE)
  }

  xx <- hutch.code::tfidf4dtm(dtm = x, force = TRUE)

  if (is.factor(y)) {
    out <- knn3(stm2dns(xx, memory.limit()), y, k = param$k, ...)
  }
  else {
    out <- knnreg(stm2dns(xx, memory.limit()), y, k = param$k, ...)
  }

  attr(out, 'original_train') <- x
  attr(out, 'used_train')     <- xx
  out
}

# predict ----------------------------------------------------------------
#
knn_cvAble$predict <- function(
  modelFit, newdata, submodels = NULL, ...
) {

  requireNamespace('hutch.code')
  requireNamespace('slam')
  if (inherits(newdata, 'data.frame')) {
    newdata <- hutch.code:::as.simple_triplet_matrix.data.frame(
      newdata,
      force = TRUE
    )
  }

  xx <- hutch.code::reweights_test(
    target   = newdata,
    original = attr(modelFit, 'original_train'),
    force   = TRUE
  )

  if (modelFit$problemType == "Classification") {
    predict(modelFit, stm2dns(xx, memory.limit()), type = "class")
  } else {
    predict(modelFit, stm2dns(xx, memory.limit()))
  }
}


# prob -------------------------------------------------------------------
#
knn_cvAble$prob <- function(
  modelFit, newdata, submodels = NULL, ...
) {
  requireNamespace('hutch.code')
  requireNamespace('slam')
  if (inherits(newdata, 'data.frame')) {
    newdata <- hutch.code:::as.simple_triplet_matrix.data.frame(
      newdata,
      force = TRUE
    )
  }

  xx <- hutch.code::reweights_test(
    target   = newdata,
    original = attr(modelFit, 'original_train'),
    force   = TRUE
  )

  predict(modelFit, stm2dns(xx, memory.limit()), type = "prob")
}

# saving -------------------------------------------------------------------
#
devtools::use_data(knn_cvAble, overwrite = TRUE)



# ==========================================================================
# ============================  rf_cvAble =================================
# ==========================================================================

rf_cvAble <- caret:::getModelInfo()$rf

# label --------------------------------------------------------------------
#
rf_cvAble$label <- stringr::str_c(
  caret:::getModelInfo()$rf$label,
  '(with possibilities to manage data in the inner resampling steps)'
)

# libraries ----------------------------------------------------------------
#
rf_cvAble$library <- c(caret:::getModelInfo()$rf$library, 'hutch.code')

# fit ----------------------------------------------------------------------
#
rf_cvAble$fit <- function(
  x, y, wts, param, lev, last, classProbs, ...) {

  requireNamespace('hutch.code')

  if (is.data.frame(x)) {
    x <- hutch.code:::as.simple_triplet_matrix.data.frame(x, force = TRUE)
  }

  xx <- hutch.code::tfidf4dtm(dtm = x, force = TRUE)

  out <- randomForest::randomForest(
           stm2dns(xx, memory.limit()), y, mtry = param$mtry, ...
  )

  attr(out, 'original_train') <- x
  attr(out, 'used_train')     <- xx
  out
}

# predict ----------------------------------------------------------------
#
rf_cvAble$predict <- function(
  modelFit, newdata, submodels = NULL, ...
) {
  if (is.null(newdata)) return(predict(modelFit))

  requireNamespace('hutch.code')
  requireNamespace('slam')
  if (inherits(newdata, 'data.frame')) {
    newdata <- hutch.code:::as.simple_triplet_matrix.data.frame(
      newdata,
      force = TRUE
    )
  }

  xx <- hutch.code::reweights_test(
    target   = newdata,
    original = attr(modelFit, 'original_train'),
    force   = TRUE
  )

  randomForest:::predict.randomForest(modelFit, stm2dns(xx, memory.limit()))
}


# prob -------------------------------------------------------------------
#
rf_cvAble$prob <- function(
  modelFit, newdata, submodels = NULL, ...
) {
  if (is.null(newdata)) return(predict(modelFit))

  requireNamespace('hutch.code')
  requireNamespace('slam')
  if (inherits(newdata, 'data.frame')) {
    newdata <- hutch.code:::as.simple_triplet_matrix.data.frame(
      newdata,
      force = TRUE
    )
  }

  xx <- hutch.code::reweights_test(
    target   = newdata,
    original = attr(modelFit, 'original_train'),
    force   = TRUE
  )

  randomForest:::predict.randomForest(
    modelFit,
    stm2dns(xx, memory.limit()),
    type = "prob"
  )
}

# saving -------------------------------------------------------------------
#
devtools::use_data(rf_cvAble, overwrite = TRUE)



# ==========================================================================
# ============================  LogitBoost_cvAble ==========================
# ==========================================================================

LogitBoost_cvAble <- caret:::getModelInfo()$LogitBoost

# label --------------------------------------------------------------------
#
LogitBoost_cvAble$label <- stringr::str_c(
  caret:::getModelInfo()$LogitBoost$label,
  '(with possibilities to manage data in the inner resampling steps)'
)

# libraries ----------------------------------------------------------------
#
LogitBoost_cvAble$library <- c(caret:::getModelInfo()$LogitBoost$library,
  'hutch.code'
)

# fit ----------------------------------------------------------------------
#
LogitBoost_cvAble$fit <- function(
  x, y, wts, param, lev, last, classProbs, ...) {

  requireNamespace('hutch.code')

  if (is.data.frame(x)) {
    x <- hutch.code:::as.simple_triplet_matrix.data.frame(x, force = TRUE)
  }

  xx <- hutch.code::tfidf4dtm(dtm = x, force = TRUE)

  caTools::LogitBoost(stm2dns(xx, memory.limit()), y, nIter = param$nIter)

  attr(out, 'original_train') <- x
  attr(out, 'used_train')     <- xx
  out
}

# predict ----------------------------------------------------------------
#
LogitBoost_cvAble$predict <- function(
  modelFit, newdata, submodels = NULL, ...
) {
  if (is.null(newdata)) return(predict(modelFit))

  requireNamespace('hutch.code')
  requireNamespace('slam')
  if (inherits(newdata, 'data.frame')) {
    newdata <- hutch.code:::as.simple_triplet_matrix.data.frame(
      newdata,
      force = TRUE
    )
  }

  xx <- hutch.code::reweights_test(
    target   = newdata,
    original = attr(modelFit, 'original_train'),
    force   = TRUE
  )

  xxx <- stm2dns(xx, memory.limit())

  out <- caTools::predict.LogitBoost(modelFit, xxx, type = "class")

  if (!is.null(submodels)) {
    tmp <- out
    out <- vector(mode = "list", length = nrow(submodels) + 1)
    out[[1]] <- tmp

    for (j in seq(along = submodels$nIter)) {
      out[[j + 1]] <- caTools::predict.LogitBoost(modelFit,
        xxx, nIter = submodels$nIter[j]
      )
    }
  }
  out
}


# prob -------------------------------------------------------------------
#
LogitBoost_cvAble$prob <- function(
  modelFit, newdata, submodels = NULL, ...
) {
  if (is.null(newdata)) return(predict(modelFit))

  requireNamespace('hutch.code')
  requireNamespace('slam')
  if (inherits(newdata, 'data.frame')) {
    newdata <- hutch.code:::as.simple_triplet_matrix.data.frame(
      newdata,
      force = TRUE
    )
  }

  xx <- hutch.code::reweights_test(
    target   = newdata,
    original = attr(modelFit, 'original_train'),
    force   = TRUE
  )

  xxx <- stm2dns(xx, memory.limit())

  out <- caTools::predict.LogitBoost(modelFit, xxx, type = "raw")
  out <- t(apply(out, 1, function(x) x/sum(x)))
  if (!is.null(submodels)) {
    tmp <- vector(mode = "list", length = nrow(submodels) +
                    1)
    tmp[[1]] <- out
    for (j in seq(along = submodels$nIter)) {
      tmpProb <- caTools::predict.LogitBoost(modelFit,
        xxx, type = "raw", nIter = submodels$nIter[j])
      tmpProb <- out <- t(apply(tmpProb, 1, function(x) x/sum(x)))
      tmp[[j + 1]] <- as.data.frame(
        tmpProb[, modelFit$obsLevels, drop = FALSE]
      )
    }
    out <- tmp
  }
  out
}

# saving -------------------------------------------------------------------
#
devtools::use_data(LogitBoost_cvAble, overwrite = TRUE)




# ==========================================================================
# ============================  glmnet_cvAble =================================
# ==========================================================================

glmnet_cvAble <- caret:::getModelInfo()$glmnet

# label --------------------------------------------------------------------
#
glmnet_cvAble$label <- stringr::str_c(
  caret:::getModelInfo()$glmnet$label,
  '(with possibilities to manage data in the inner resampling steps)'
)

# libraries ----------------------------------------------------------------
#
glmnet_cvAble$library <- c(caret:::getModelInfo()$glmnet$library,
  'hutch.code'
)

# fit ----------------------------------------------------------------------
#
glmnet_cvAble$fit <- function(
  x, y, wts, param, lev, last, classProbs, ...) {

  requireNamespace('hutch.code')

  if (is.data.frame(x)) {
    x <- hutch.code:::as.simple_triplet_matrix.data.frame(x, force = TRUE)
  }

  xx <- hutch.code::tfidf4dtm(dtm = x, force = TRUE)
  xxx <- stm2dns(xx, memory.limit())

  numLev <- if (is.character(y) | is.factor(y))
    length(levels(y))
  else NA
  theDots <- list(...)
  if (all(names(theDots) != "family")) {
    if (!is.na(numLev)) {
      fam <- ifelse(numLev > 2, "multinomial", "binomial")
    }
    else fam <- "gaussian"
    theDots$family <- fam
  }
  if (!is.null(wts))
    theDots$weights <- wts
  if (!(class(xxx)[1] %in% c("matrix", "sparseMatrix")))
    xxx <- as.matrix(xxx)
  modelArgs <- c(list(x = xxx, y = y, alpha = param$alpha), theDots)
  out <- do.call("glmnet", modelArgs)
  if (!is.na(param$lambda[1]))
    out$lambdaOpt <- param$lambda[1]

  attr(out, 'original_train') <- x
  attr(out, 'used_train')     <- xx
  out
}

# predict ----------------------------------------------------------------
#
glmnet_cvAble$predict <- function(
  modelFit, newdata, submodels = NULL, ...
) {
  if (is.null(newdata)) return(predict(modelFit))

  requireNamespace('hutch.code')
  requireNamespace('slam')
  if (inherits(newdata, 'data.frame')) {
    newdata <- hutch.code:::as.simple_triplet_matrix.data.frame(
      newdata,
      force = TRUE
    )
  }

  xx <- hutch.code::reweights_test(
    target   = newdata,
    original = attr(modelFit, 'original_train'),
    force   = TRUE
  )
  xxx <- stm2dns(xx, memory.limit())

  if (length(modelFit$obsLevels) < 2) {
    out <- predict(modelFit, xxx, s = modelFit$lambdaOpt)
  } else {
    out <- predict(modelFit, xxx, s = modelFit$lambdaOpt,
      type = "class")
  }
  if (is.matrix(out))
    out <- out[, 1]
  if (!is.null(submodels)) {
    if (length(modelFit$obsLevels) < 2) {
      tmp <- as.list(as.data.frame(predict(modelFit, xxx,
        s = submodels$lambda)))
    } else {
      tmp <- predict(modelFit, xxx, s = submodels$lambda,
        type = "class")
      tmp <- if (is.matrix(tmp))
        as.data.frame(tmp, stringsAsFactors = FALSE)
      else as.character(tmp)
      tmp <- as.list(tmp)
    }
    out <- c(list(out), tmp)
  }
  out
}


# prob -------------------------------------------------------------------
#
glmnet_cvAble$prob <- function(
  modelFit, newdata, submodels = NULL, ...
) {
  if (is.null(newdata)) return(predict(modelFit))

  requireNamespace('hutch.code')
  requireNamespace('slam')
  if (inherits(newdata, 'data.frame')) {
    newdata <- hutch.code:::as.simple_triplet_matrix.data.frame(
      newdata,
      force = TRUE
    )
  }

  xx <- hutch.code::reweights_test(
    target   = newdata,
    original = attr(modelFit, 'original_train'),
    force   = TRUE
  )
  xxx <- stm2dns(xx, memory.limit())

  obsLevels <- if ("classnames" %in% names(modelFit))
    modelFit$classnames
  else NULL
  probs <- predict(modelFit, xxx, s = modelFit$lambdaOpt, type = "response")
  if (length(obsLevels) == 2) {
    probs <- as.vector(probs)
    probs <- as.data.frame(cbind(1 - probs, probs))
    colnames(probs) <- modelFit$obsLevels
  } else {
    probs <- as.data.frame(probs[, , 1, drop = FALSE])
    names(probs) <- modelFit$obsLevels
  }
  if (!is.null(submodels)) {
    tmp <- predict(modelFit, xxx, s = submodels$lambda, type = "response")
    if (length(obsLevels) == 2) {
      tmp <- as.list(as.data.frame(tmp))
      tmp <- lapply(tmp, function(x, lev) {
        xxx <- as.vector(xxx)
        tmp <- data.frame(1 - xxx, xxx)
        names(tmp) <- lev
        tmp
      }, lev = modelFit$obsLevels)
    } else {
      tmp <- apply(tmp, 3, function(x) data.frame(x))
    }
    probs <- if (is.list(tmp)) c(list(probs), tmp) else list(probs, tmp)
  }
  probs
}

# saving -------------------------------------------------------------------
#
devtools::use_data(glmnet_cvAble, overwrite = TRUE)





















