#' #' # Package and relative options and settings
#' #  =========================================================================
#' pckg_need <- c("glmnet","randomForest","class","gam","gbm","nnet","polspline",
#'   "MASS", "e1071","stepPlr","arm","party","spls","LogicReg","nnls",
#'   "SIS","BayesTree","quadprog","ipred","mlbench","rpart","caret",
#'   "mda", "earth", 'rminer', 'pacman', 'survival', 'TeachingDemos',
#'   'gplots', 'plotrix', 'plotmo', 'foreach', 'Matrix', 'parallel',
#'   'splines', 'log4r', 'progress', 'pryr', 'e1071', 'devtools',
#'   'glmnet', 'caret', 'tidyverse', 'SnowballC', 'RWeka', 'unbalanced',
#'   'tm', 'NLP', 'slam'
#' )
#'
#' if (!requireNamespace('purrr', quietly = TRUE)) {
#'   install.packages('purrr')
#' }
#' pckg_to_install <- purrr::map_lgl(pckg_need,
#'   ~ !requireNamespace(., quietly = TRUE)
#' )
#'
#' if (sum(pckg_to_install)) {
#'   stop('The following package(s) have to be installed: ',
#'        paste(pckg_need[pckg_to_install], collapse = ', '),
#'        '.'
#'   )
#' }
#' install.packages(pckg_need[pckg_to_install], dependencies = TRUE)

#' java max usable memory has to be set before java machine starts

  options(java.parameters = "-Xmx40g")
library(caret, quietly = TRUE) # mlt facilites -----------------------------
library(e1071)
# library(glmnet)
library(Matrix)
# library(randomForest)

library(costumer) # package of the project -------------------------------

library(tidyverse, quietly = TRUE) # Hadley verse --------------------------
  library(stringr)
  library(pryr)

library(tm)
library(pROC)


library(parallel) # parallel computing -------------------------------------
# devtools::install_github("hadley/multidplyr")
library(multidplyr)
# n_cores <- max(1, detectCores() - 1)
#   cl      <- makeCluster(n_cores)
#   on.exit(stopCluster(cl), add = TRUE)
cluster <- create_cluster() # by default max(2, n - 1)
#
set_default_cluster(cluster)
cluster_eval(cluster, options(java.parameters = "-Xmx40g"))


library(progress) # progress bar on console --------------------------------

library(log4r) # log file --------------------------------------------------
  pathlog    <- file.path(getwd(), 'log')

  costumer:::loggeRs(prj_name = 'hutch', pathlog = pathlog)
  #
  log4r::info(logger.1, 'HUTCH main code - start')
  log4r::info(logger.1, capture.output(devtools::session_info()))


#' # Data Import
#'
#' Training negatives from pubmed, all ct.gov and positive marks for test
#' on ct.gov
#'
data("summaries_pubmed")
# sr10_kourbeti <- summaries_pubmed$sr10_kourbeti
# rm(summaries_pubmed)

#' =============================================================================
#' Create_train() and create_test()
#'
corpus_time <- system.time(
  corpus_mem <- pryr::mem_change({
    #
    corpus_train <- set_names(
      map(seq_along(summaries_pubmed),#13, # 13 = Liu #2, # 2 = kourbeti #
        ~ create_train(
          summaries_pubmed[[.]],
          name_df = names(summaries_pubmed[.])
          )
      ),
      names(summaries_pubmed)
    )
    log4r::info(logger.1, 'corpus_train ready')
    #
  })
)

log4r::info(logger.1, str_c(
  'corpora created in ', corpus_time, ' of time and ', corpus_mem, ' of RAM.'
))

#' =============================================================================
#' Preprocessing train and test and DTM creation
#'

#' progressbar -----------------------------------------------------------------
#
pb <- progress::progress_bar$new(
  format = " processing :what [:bar] :percent in :elapsed",
  total  = length(corpus_train) * 5 + 4,
  clear  = FALSE,
  width  = 76
)
tick <- function(x, pb, token = '', ...) {
  pb$tick(tokens = list(what = token), ...)
  invisible(x)
}

#' preprocess ------------------------------------------------------------------
#
log4r::info(logger.1, 'preproc train - start')
invisible(pb$tick(0, tokens = list(what = 'training_data')))

preproc_time <- system.time(
  preproc_mem <- pryr::mem_change({
    #
    train_data <- map(seq_along(corpus_train), ~{
      corpus_train[[.]]%>%

      tick(pb, 'lowering') %>%
      lowering %>%

      tick(pb, 'nonword') %>%
      nonword %>%

      tick(pb, 'stem') %>%
      stem(language = "english") %>%

      tick(pb, 'white') %>%
      white %>%

      tick(pb, 'ngram') %>%
      ngram
    })
    #
  })
)

log4r::info(logger.1, str_c(
  'data preprocessed in ', preproc_time, ' of time and ', preproc_mem,
  ' of RAM.'
))
invisible(pb$tick(tokens = list(what = 'End')))
# beepr::beep(8)

#' dtm creation ---------------------------------------------------------------
#'
log4r::info(logger.1, 'train_dtm - start')
invisible(pb$tick(token = list(what = 'train_dtm')))

dtm_time <- system.time({
  #
  train_dtm <- map(seq_along(train_data),
      ~ atom_dtm(train_data[[.]], parallel = FALSE)
  )
  log4r::info(logger.1, 'train_dtm ready')
  #
})


log4r::info(logger.1, str_c(
  'train_dtm preprocessed in ', preproc_time,
  ' of time (RAM used NA because of parallel computation)'
))

# beepr::beep(8)

names(train_dtm) <- names(summaries_pubmed)#[[13]] # 13 = Liu # 2 = kourbeti

# train_kourbeti_dtm <- train_dtm
#train_liu_dtm <- train_dtm


#' Store data description in a text file
out_file <- file('Hutch2_DTMs-train_info.txt', open = 'wt')
  sink(file = out_file)
  sink(file = out_file, type = 'message')
    walk2(.x = names(train_dtm), .y = train_dtm, ~ {
      message(.x)
      print(.y)
      message()
    })
  sink(type = 'message')
  sink()
close(out_file)



invisible(pb$tick(token = list(what = 'saving train_dtm')))
# devtools::use_data(train_kourbeti_dtm, overwrite = TRUE, compress = 'xz')

invisible(pb$tick(token = list(what = 'end')))



#' =============================================================================
#' map it all
#'
# data("train_dtm")

pb <- progress::progress_bar$new(
  format = " processing :what [:bar] :percent in :elapsed",
  total  = length(train_dtm) * 3 + 4,
  clear  = FALSE,
  width  = 76
)
tick <- function(x, pb, token = '', ...) {
  pb$tick(tokens = list(what = token), ...)
  invisible(x)
}

log4r::info(logger.1, 'main HUTCH - start')
invisible(pb$tick(0, tokens = list(what = 'set parameters and datas')))

hutch_parameter <- list(
  max_retained = (4/100), # 37589 * 2/10,
  seed          = 14,
  n_cv_folds    = 5,
  tuneLenght    = 10
)

safe_train <- purrr::safely(caret::train)

invisible(pb$tick(tokens = list(what = 'setup dataset(s)')))

hutch_time <- system.time(
  hutch_mem <- pryr::mem_change(
    hutch <- data_frame(

      ## meta-analyses
      #
      original_dtm = setNames(map(names(train_dtm), ~{

        list(
          data   = train_dtm[[.]],
          labels = summaries_pubmed[[.]]$final %>% factor(
            levels = c(0L, 1L), labels = c('excluded', 'included')
          )
        )
      }), names(train_dtm)) %>%
        tick(pb, 'reduce dataset'),

      ## data_used
      #
      data_used = map(original_dtm,
        ~ {
          out <- variable_select(.[['data']],
            max_retained = as.integer(.[['data']]$ncol * hutch_parameter[['max_retained']])
          )
          if (any(slam::row_sums(out) == 0)) {
            warning(paste0('With this size of max_retained (', max_retained,
              ') there are documents with any of the token retained.',
              'Increase it.'
            ))
          }
          out
        }
      ) %>%
        tick(pb, 'setup MLTs methods'),

      ## MLTs
      #
      mlts = map(setNames(names(original_dtm), names(original_dtm)),
        ~ list(
           svm = data("svmLinear2_cvAble", package = 'costumer') %>%
             parse(text = .) %>%
             eval()#,
           # knn = data("knn_cvAble", package = 'costumer') %>%
           #   parse(text = .) %>%
           #   eval(),
           # naive_bayes = data("nb_cvAble", package = 'costumer') %>%
           #   parse(text = .) %>%
           #   eval(),
           # rf = data("rf_cvAble", package = 'costumer') %>%
           #  parse(text = .) %>%
           #  eval(),
           # logitboost = data("LogitBoost_cvAble", package = 'costumer') %>%
           #   parse(text = .) %>%
           #   eval(),
          # glmnet = data("glmnet_cvAble", package = 'costumer') %>%
          #  parse(text = .) %>%
          #  eval()
        )
      ) %>%
        tick(pb, 'setup Balance methods'),

      ## Balance managements
      #
      balance_methods = map(
        setNames(names(original_dtm), names(original_dtm)),
        ~ list(
            none    = NULL,
            # ROS5050_new = data("ROS5050_new", package = 'costumer') %>%
            #   parse(text = .) %>%
            #   eval(),
            # RUS5050_new = data("RUS5050_new", package = 'costumer') %>%
            #   parse(text = .) %>%
            #   eval(),
            # ROS3565_new = data("ROS3565_new", package = 'costumer') %>%
            #  parse(text = .) %>%
            #  eval(),
            RUS3565_new = data("RUS3565_new", package = 'costumer') %>%
             parse(text = .) %>%
             eval()
        )
      )
    )))

n_mods <- length(hutch[[1]]) * length(hutch[[3]][[1]]) * length(hutch[[4]][[1]])




hutch2 <- data_frame(
  group = map_chr(setNames(seq_len(n_mods), paste0('mod-', seq_len(n_mods))), ~ .),
  name = rep(names(hutch[[1]]), each = n_mods/length(hutch[[1]])),
  original_dtm    = map(name, ~ hutch$original_dtm[[.]]),
  data_used       = map(name, ~ hutch$data_used[[.]]),
  mlts            = map(
    setNames(rep(seq_along(hutch[[3]][[1]]), n_mods/length(hutch[[3]][[1]])),
             names(hutch[[3]][[1]][rep(seq_along(hutch[[3]][[1]]), n_mods/length(hutch[[3]][[1]]))])),
    ~ hutch[[3]][[1]][[.]]
  ),
  balance_methods = map(
    setNames(rep(seq_along(hutch[[4]][[1]]), n_mods/length(hutch[[4]][[1]])),
             names(hutch[[4]][[1]][rep(seq_along(hutch[[4]][[1]]), n_mods/length(hutch[[4]][[1]]))])),
    ~ hutch[[4]][[1]][[.]]
  )
)

#hutch2_kourbeti <- hutch2
# hutch2_liu <- hutch2
# beepr::beep(8)
# save(hutch2_kourbeti, file = './output/hutch2_kourbeti.rda')
# devtools::use_data(hutch2_kourbeti, overwrite = TRUE)

# devtools::use_data(hutch2, overwrite = TRUE)
# load('data/hutch2.rda')

#' Store data description in a text file
old_opt <- options(tibble.print_max = Inf)
out_file <- file('Hutch2_model-matrix.txt', open = 'wt')
  sink(file = out_file)
  sink(file = out_file, type = 'message')
    print(hutch2)
    message()
  sink(type = 'message')
  sink()
close(out_file)
options(old_opt)






## distribute groups trought the clusters
#
# load('./output/hutch2_kourbeti.rda')

summary_auc <- function(dat, lev = NULL, model = NULL) {
  #  message(paste('summary input ', str(dat)))
  out <- pROC::auc(
    dat[['obs']], #%>% as.integer,
    dat[['pred']] %>%
      # factor %>%
      as.integer
  ) %>% as.double
  names(out) <- 'AUC'
  #  message(paste('summary output ', str(out)))
  out
}

# hutch_cl <- hutch2_liu %>%
hutch_cl <- hutch2 %>%
  #  dplyr::filter(group %in% c(3, 5, 7, 9, 11, 19)) %>%
  partition(group)

hutch_cl %>%
  ## Assign libraries
  cluster_library("devtools") %>%
  cluster_library("log4r") %>%
  cluster_library("tidyverse") %>%
  cluster_library("stringr") %>%
  cluster_library("lubridate") %>%
  cluster_library("caret") %>%
  cluster_library("e1071") %>%
  cluster_library("glmnet") %>%
  cluster_library("Matrix") %>%
  cluster_library("randomForest") %>%
  cluster_library("costumer") %>%
  cluster_library("unbalanced") %>%
  cluster_library("magrittr") %>%
  cluster_library("tm") %>%
  cluster_library('slam')


hutch_cl %>%
  cluster_assign_value('safe_train', safe_train) %>%
  cluster_assign_value('hutch_parameter', hutch_parameter) %>%
  cluster_assign_value('summary_auc', summary_auc) %>%
  cluster_assign_value('logger.1', logger.1)




hutch_cl3 <- hutch_cl %>%
  mutate(
    model = map(group, ~ {
      set.seed(hutch_parameter$seed)
      model_time <- system.time(
        model_mem <- pryr::mem_change({
          out <- safe_train(
            ## data
            #
            x = data_used[[which(group == .)]] %>% as.data.frame(),
            y = original_dtm[[which(group == .)]][['labels']],

            ## method
            #
            method       = mlts[[which(group == .)]],

            ## metric
            #
            metric = 'AUC',
            maximize = TRUE,

            ## control
            #
            trControl = trainControl(
              method            = 'cv',
              number            = hutch_parameter$n_cv_folds,
              search            = 'random',
              savePredictions   = 'final',
              classProbs        = TRUE,
              selectionFunction = 'best', #oneSE
              sampling          = balance_methods[[which(group == .)]],
              summaryFunction   = summary_auc,
              seeds             = c(
                map(
                  seq_len(hutch_parameter$n_cv_folds),
                  ~rep.int(hutch_parameter$seed, hutch_parameter$tuneLenght)
                ),
                hutch_parameter$seed
              )
            ),

            ## tuning
            #
            tuneLength = hutch_parameter$tuneLenght
            #tuneGrid = data.frame(mtry = 30)
          )
        })
      )
      log4r::info(logger.1, paste0(
        'model_', .,
        ': DONE in ', model_time[[1]], ' (s) ',
        ' and using ', model_mem, ' (RAM).)'
      ))
      eval(parse(text = paste0(
        'model_', ., '_',
        name[[which(group == .)]], '_',
        names(mlts[which(group == .)]), '_',
        names(balance_methods[which(group == .)]),
        ' <- out'
      )))
      eval(parse(text = paste0(
        'devtools::use_data(model_', ., '_',
        name[[which(group == .)]], '_',
        names(mlts[which(group == .)]), '_',
        names(balance_methods[which(group == .)]),
        ', overwrite = TRUE)'
      )))
      out
    }
    )
  )


hutch3 <- collect(hutch_cl3)

log4r::info(logger.1, paste('All DONE in', model_time, ' (time) ',
  ' and using ', model_mem, ' (RAM).'))
pb$tick(tokens = list(what = 'END'))


#' =============================================================================
#' plot and tables for statistics
#'
# hutch3[map_lgl(hutch3$model, ~ !is.null(.$result)),]$mlts %>% str(2)
# map(hutch3[map_lgl(hutch3$model, ~ is.null(.$result)),]$model, ~.$error)



#' =============================================================================
#' confusion matrices
#'
hutch3 <- hutch3 %>%
  mutate(
    cfm  = model %>% map(~.$result %>% confusionMatrix(norm = 'none')),
    what = paste(
      name,
      mlts %>%
        map_chr(~ .$label %>% str_extract('([^w\\(]|[- ])+')),
      group %>%
        map_chr(~ hutch2[hutch2$group == ., ][['balance_methods']] %>% names),
      sep = ' - '
    ),
    plots = map2(.x = model, .y = what,
      ~ plot.train(.$result, nameInStrip = TRUE, main = .y)
    )
)

devtools::use_data(hutch3, overwrite = TRUE)


out_lile <- file('HUTCH2-ROCs.txt', open = 'wt')
  sink(file = out_lile)
  sink(file = out_lile, type = 'message')
    walk2(.x = hutch3$what, .y = hutch3$cfm, ~ {
      message(.x)
      print(.y)
  })
  sink(type = 'message')
  sink()
close(out_lile)


walk(hutch3$plots, ~{
  png(paste(.$main %>% str_replace_all(' ', ''), '.png'))
  print(.)
  dev.off()
})



## predict all
# Systematic Reviews ===========================================================
path_to_file <- file.path('data-raw', 'raw_pubmed')
pubmed_files <- list.files(path_to_file)

order_res <- pubmed_files %>%
  str_extract('[0-9]+') %>%
  as.integer() %>%
  order

pubmed_files <- pubmed_files[order_res]


res_pubmed <- purrr:::map(file.path(path_to_file, pubmed_files), ~ {
  data <- readxl::read_xlsx(., sheet = 2) %>%
    distinct(NCT, .keep_all = TRUE)
  names(data) <- tolower(names(data))
  data
})

names(res_pubmed) <- tolower(pubmed_files) %>%
  str_extract('^[^\\.]*')



# load('path/to/test_corpus_preproc.rda') OR data(test_corpus_preproc)
duplicate_test <- test_corpus_preproc %>%
  names %>%
  duplicated

test_corpus_preproc <- test_corpus_preproc[!duplicate_test]

gc(TRUE)
gc(TRUE)

dtm_to_test <- atom_dtm(test_corpus_preproc, step = 500L, parallel = FALSE)



test_names <- names(test_corpus_preproc)




hutch3 <- hutch3 %>%
  ungroup %>%
  mutate(
    res_pubmed = map(name, ~res_pubmed[[.]]),
    res_gold   = map(res_pubmed, ~ setNames(as.integer(.$final > 1), .$nct))
  )


hutch3 <- hutch3 %>%
  mutate(
    test_dtm     = map(.x = data_used, ~{
      dtm_to_test %>%
        dtm_lfilter(.x) %>%
        reweights_test(.x)
    })
)


devtools::use_data(hutch3, overwrite = TRUE)



# predict ======================================================================

make_prediction <- function(model, newX, threshold = 0, d = 1000L) {

  modelFit <- model[['result']][['finalModel']]
  nn     <- dim(newX)[[1]]
  breaks <- (seq_len(nn) %>% split(ceiling(seq_along(.)/d)))
  n_step <- length(breaks)

  pb_all <- progress::progress_bar$new(
    format = ":what [:bar] :percent in :elapsed",
    total  = n_step,
    width  = 76
  )
  invisible(pb_all$tick(0, tokens = list(what = 'predictions')))

  invisible(
    map(.x = breaks, ~{
      prediction <- e1071:::predict.svm(
        modelFit,
        costumer:::stm2dns(newX[.x, ], memory.limit()),
        probability = TRUE
      )

      pb_all$tick(tokens = list(what = 'predictions'))

      set_names(as.integer(prediction) - 1L, names(prediction))
    }) %>%
    unlist  %>%
    factor(levels = c(0L, 1L), labels = (c('excluded', 'included'))) %>%
    set_names(str_replace(names(.), '^[0-9]+\\.', ''))
    # (attr(a, 'probabilities')[, 'included'] > threshold) %>%
    #   factor(levels = c(FALSE, TRUE), labels = (c('excluded', 'included')))
  )
}





pb_mod <- progress::progress_bar$new(
  format = ":what [:bar] :percent in :elapsed",
  total  = length(hutch3$model),
  width  = 76
)
invisible(pb_mod$tick(0, tokens = list(what = 'models')))

hutch3 <-  hutch3 %>%
  mutate(
    predictions = map2(.x = model, .y = test_dtm, ~{
      pb_mod$tick(tokens = list(what = 'model'))
      make_prediction(.x, .y)
    })
  )

hutch3$res_gold[[1]]

hutch3 <- hutch3 %>%
  mutate(
    to_test_all = map(res_pubmed, ~ (test_names %in% .[['nct']]) %>%
      factor(levels = c(FALSE, TRUE), labels = c('excluded', 'included')) %>%
        set_names(test_names)
    ),
    to_test_gold = map(res_pubmed, ~ (test_names %in% filter(., final == 2)[['nct']]) %>%
      factor(levels = c(FALSE, TRUE), labels = c('excluded', 'included')) %>%
        set_names(test_names)
    ),
    n_true_all = map_int(to_test_all, ~ sum(. == 'included')),
    n_true_gold = map_int(to_test_gold, ~ sum(. == 'included'))
  )



hutch3 <- hutch3 %>%
  mutate(
    cfm_pred_all = map2(predictions, to_test_all, ~{
      confusionMatrix(
        data      = .x,
        reference = .y,
        positive  = 'included'
      )
    }),
    auc_pred_all = map2(predictions, to_test_all, ~
      auc(
        .y %>% as.integer,
        .x %>% as.integer
      )
    ),
    cfm_pred_gold = map2(predictions, to_test_gold, ~{
      confusionMatrix(
        data      = .x,
        reference = .y,
        positive  = 'included'
      )
    }),
    auc_pred_gold = map2(predictions, to_test_gold, ~
      auc(
        .y %>% as.integer,
        .x %>% as.integer
      )
    )
  )


devtools::use_data(hutch3, overwrite = TRUE)


suppressWarnings(suppressMessages(
  results <- data_frame(
    sr    = hutch3$what %>% str_replace('Support Vector Machines', 'SVM'),
    train_pos = map_int(hutch3$model, ~try(
      .$result$pred %>%
        as.tibble %>%
        distinct(rowIndex, .keep_all = TRUE) %>%
        select(obs) %>%
        table %>%
        `[[`(2)
    ) %>% as.integer),
    train_neg = map_int(hutch3$model, ~try(
      .$result$pred %>%
        as.tibble %>%
        distinct(rowIndex, .keep_all = TRUE) %>%
        select(obs) %>%
        table %>%
        `[[`(1)
      ) %>% as.integer),
    test_pos_all = hutch3$n_true_all,
    test_neg_all = length(hutch3$to_test_all[[1]]) - test_pos_all,
    test_pred_pos = map_int(hutch3$predictions, ~ (. == 'included') %>% sum),
    AUC    = map_dbl(hutch3$auc_pred_all, as.double) %>% round(4),
    PREV   = map_dbl(hutch3$cfm_pred_all, ~try(
      .$byClass[[8]]) %>% as.double %>% round(4)
    ),
    PPV    = map_dbl(hutch3$cfm_pred_all, ~try(
      .$byClass[[3]]) %>% as.double %>% round(4)
    ),
    SENS   = map_dbl(hutch3$cfm_pred_all, ~try(
      .$byClass[[1]]) %>% as.double %>% round(4)
    ),
    SPEC   = map_dbl(hutch3$cfm_pred_all, ~try(
      .$byClass[[2]]) %>% as.double %>% round(4)
    ),
    RECALL = map_dbl(hutch3$cfm_pred_all, ~try(
      .$byClass[[6]]) %>% as.double %>% round(4)
    ),
    `RV+`  = (SENS / (1 - SPEC)) %>% round(4),
    `RV-`  = ((1-SENS) / SPEC) %>% round(4)
  )
))
results %>%
  arrange(desc(sr)) %>%
  as.data.frame()
results$AUC %>% median(na.rm = TRUE)
results$AUC %>% mean(na.rm = TRUE)
map_dbl(results[6:11], median, na.rm = TRUE)

results$`RV+` %>% median(na.rm = TRUE)
results$`RV-` %>% median(na.rm = TRUE)
