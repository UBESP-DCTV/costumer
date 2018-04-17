
jsm_trainMLT <- function(
    dtm       = DTM,
    y         = NULL,
    virgin    = TRUE,
    MLT       = 'BOOSTING',
	reduct    = FALSE,
	threshold = NA,
	TfIdf     = FALSE,
    nIter     = 100L,
    ...
){
    if (is_null(y)) stop('Response variable "y" must be provided.')
    #
    ## We MUST have train and set in order both into dtm and in labels
    #
    n.train <- sum(label != 0)
    n.test  <- sum(label == 0)
    trainSize = 1:n.train
    testSize  = (n.train + 1):(n.train + n.test)
    #
	if(TfIdf){
		dtm <- tfidf(dtm)
	}
	#
	dtm_train <- dtm[trainSize, ]
	dtm_test  <- dtm[testSize, ]
    ## dimensionality reduction
    #
	if (reduct && !is.na(threshold) && threshold > 0){
#
log4r::info(logger.1, paste0('Terms considered only if in more then ', threshold, ' Documents'))
#
		term_select <- col_sums(dtm_train != 0) > threshold
		dtm_train <- dtm_train[, term_select]
		dtm_test  <- dtm_test[, term_select]
	} else {
        if (reduct && is.na(threshold)){
#
log4r::info(logger.1, paste0('Automatical definition of thresold for number'))
log4r::info(logger.1, paste0('document in which a term has to be present'))
#
            cs <- col_sums( dtm_train!=0)
            sizes <- vapply(0:100, function(n) dtm_train[, cs > n]$ncol, integer(1))
            delta0 <- sqrt(((0:100)/100)^2 + (sizes/max(sizes))^2)
            min_d0 <- which.min(delta0)
            #
#
log4r::info(logger.1, paste0('threshold is ', min_d0 - 1))
log4r::info(logger.1, paste0('so, a terms will be considered only if in more'))
log4r::info(logger.1, paste0('then ', min_d0 - 1))
#
            term_select <- col_sums(dtm_train != 0) > (min_d0 - 1)
            dtm_train <- dtm_train[, term_select]
            dtm_test  <- dtm_test[, term_select]
        }
    }
    #
    train_code   <- factor(label[trainSize])
    test_code    <- if(!virgin){factor(label[testSize])} else {as.factor(rep(NA, length(testSize)))}
    column_names <- colnames(dtm)
    rm(dtm)
    gc()
    #
#
#log4r::info(logger.1, 'container creation')
#
    ## NOTE: we explicitely costruct container using (part of) original RTextTools
    ##       function. I.e. we do not find a reason for the creation of data_matrix
    ##       object, which cause an overflow of integer when R try to produce it
    ##       because of the great ammount of data. Moreover as.compressd.matrix
    ##       by maxent do not have a specific method for simple triplet matrix
    ##       and coerce them to original matrix each time: very time and memory
    ##       consuming.
    #
    # matrix_train_predict <- tryCatch(
        # ## create a maxent::as.compressed.matrix in a fast way
        # #
        # expr    = if (!TfIdf) csr_dtm(dtm_train) else csr_dtmTfIdf(dtm_train),
        # #
        # warning = function(w) {
                    # log4r::warn(
                        # logger.1,
                        # paste0('matrix_train_predict: ', w)
                    # )
                    # warning(w)
                  # },
        # error   = function(e) {
                    # log4r::error(
                        # logger.1,
                        # paste0('matrix_train_predict: ', e)
                    # )
                    # stop(e)
                  # }
    # )
    # #
    save(dtm_train, file = file.path(pathoutput, 'dtm_train_temp.rda'))
    # save(
        # matrix_train_predict,
        # file = file.path(pathoutput, 'matrix_train_predict_temp.rda')
    # )
    # rm(dtm_train, matrix_train_predict)
    # gc()
    # #
    # matrix_test_predict  <- tryCatch(
        # ## create a maxent::as.compressed.matrix in a fast way
        # #
        # expr    = if (!TfIdf) csr_dtm(dtm_test) else csr_dtmTfIdf(dtm_test),
        # #
        # warning = function(w) {
                    # log4r::warn(
                        # logger.1,
                        # paste0('matrix_test_predict: ', w)
                    # )
                    # warning(w)
                  # },
        # error   = function(e) {
                    # log4r::error(
                        # logger.1,
                        # paste0('matrix_test_predict: ', e)
                    # )
                    # stop(e)
                  # }
    # )
    # #
    save(dtm_test, file = file.path(pathoutput, 'dtm_test_temp.rda'))
    rm(dtm_test)
    # gc()
    # load(file.path(pathoutput, 'matrix_train_predict_temp.rda'))
    # #
    # container <- new("matrix_container",
        # training_matrix       = matrix_train_predict,
        # classification_matrix = matrix_test_predict,
        # training_codes        = train_code,
        # testing_codes         = test_code,
        # column_names          = column_names,
        # virgin                = virgin
    # )

    #-----------------------------------------------------------------------
    # container <- create_container(
        # matrix    = dtm,
        # labels    = label,
        # trainSize = 1:n.train,
        # testSize  = (n.train + 1):(n.train + n.test),
        # virgin    = virgin
    # )
    #-----------------------------------------------------------------------

    # save(container, file = file.path(pathoutput, 'container_temp.rda'))
    # rm(matrix_train_predict, matrix_test_predict, container)
    # gc()
# #
log4r::info(logger.1, 'train MLT')
#
    ## auxiliary function
	#
	extract_maximum_prob          <- function(x) x[which.max(x)]
	extract_label_from_prob       <- function(x) which.max(x)
	extract_label_from_prob_names <- function(x) rownames(as.matrix(which.max(x)))
	#

if(MLT == 'RF'){
log4r::info(logger.1, 'RandomForest algoritm processing')
log4r::info(logger.1, 'create train matrix')
#
#    load(file.path(pathoutput, 'dtm_train_temp.rda'))
    #
    dns_train <- stm_dns(dtm_train)
	#
	rm(dtm_train)
	gc()
log4r::info(logger.1, 'create RandomForest model')
#
	#
	memory.limit(
		max(memory.limit(), 5 * pryr::object_size(dns_train))
	)
	#
	model <- randomForest::randomForest(
				x = dns_train,
				y = train_code,
				ntree = ntree,
				do.trace = TRUE
			 )
	#
	save(model, file = file.path(pathoutput, 'rf_model_temp.rda'))
	rm(model)
	gc()
	#
#
log4r::info(logger.1, 'create test matrix')
#
	load(file.path(pathoutput, 'dtm_test_temp.rda'))
	#
	dns_test <- stm_dns(dtm_test)
	#
	rm(dtm_test)
	gc()
#
log4r::info(logger.1, 'create predictions and probabilities')
#
	load(file.path(pathoutput, 'rf_model_temp.rda'))
	rf_results <- predict(model, newdata = dns_test, type = "prob", ...)
	#
	rf_pred <- apply(rf_results, 1, extract_label_from_prob_names)
    rf_prob <- apply(rf_results, 1, extract_maximum_prob)
#
log4r::info(logger.1, 'model classification')
#
   c_model <- data.frame(
				RF_LABEL = as.character(rf_pred),
				RF_PROB  = rf_prob
	)
	#

} else
#
if(MLT == 'BOOSTING'){
log4r::info(logger.1, 'boosting algoritm processing')
log4r::info(logger.1, 'create train matrix')
#
#    load(file.path(pathoutput, 'dtm_train_temp.rda'))
    #
    dns_train <- stm_dns(dtm_train)
	#
	rm(dtm_train)
	gc()

	#-------------------------------------------------------------------
	# model <- train_model(
		# container = container,
		# algorithm = MLT,
		# ...
	# )
	#-------------------------------------------------------------------

#
log4r::info(logger.1, 'create LogitBoosting model')
#
	#
	memory.limit(
		max(memory.limit(), 5 * pryr::object_size(dns_train))
	)
	#
	model <- caTools::LogitBoost(
				xlearn = dns_train,
				ylearn = train_code,
				nIter = nIter
			 )
	#
	save(model, file = file.path(pathoutput, 'model_temp.rda'))
	rm(model)
	gc()
	#
#
log4r::info(logger.1, 'create test matrix')
#
	load(file.path(pathoutput, 'dtm_test_temp.rda'))
	#
	dns_test <- stm_dns(dtm_test)
	#
	rm(dtm_test)
	gc()

	#-------------------------------------------------------------------
	# c_model <- classify_model(
		# container = container,
		# model     = model
	# )
	#-------------------------------------------------------------------

#
log4r::info(logger.1, 'create predictions and probabilities')
#
	load(file.path(pathoutput, 'model_temp.rda'))
	lboost_results <- predict(model, xtest = dns_test, type = "raw")
	#
	lboost_pred <- apply(lboost_results, 1, extract_label_from_prob_names)
	lboost_prob <- apply(lboost_results, 1, extract_maximum_prob)
	#
#
log4r::info(logger.1, 'model classification')
#
   c_model <- data.frame(
				LOGITBOOST_LABEL = as.character(lboost_pred),
				LOGITBOOST_PROB  = lboost_prob
	)
	#
}

#
log4r::info(logger.1, 'return results')
#
	load(file.path(pathoutput, 'container_temp.rda'))
	list( # return
#		container        = container,
		train_model      = model,
		classify_model   = c_model,
		features         = column_names
	)

#
}
#
##=============================================================================
#
## README  :
#
## NOTE    :
#
## EXAMPLES:
#
###############################################################################
