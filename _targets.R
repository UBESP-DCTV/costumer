#' java max usable memory has to be set before java machine starts
options(
  tidyverse.quiet = TRUE,
  java.parameters = "-Xmx40g"
)

library(future)
library(future.callr)
future::plan(future.callr::callr)

library(Matrix)

library(targets)
library(tarchetypes)
library(here)


# load costumer functions
invisible(lapply(list.files("R", "\\.R", full.names = TRUE), source))
source(here("data-raw/caret_methods_cvAble.R"))
source(here("data-raw/data_RUSROS_new.R"))


# https://github.com/ropensci/targets/discussions/598
read_from_path <- function(path) {
  envir <- environment()
  data_name <- load(path, envir = envir)
  get(data_name)
}

summary_auc <- function(dat, lev = NULL, model = NULL) {
  pROC::auc(
    dat[['obs']],
    as.integer(dat[['pred']])
  ) |>
    as.double() |>
    purrr::set_names("AUC")
}

make_prediction <- function(model, newX, threshold = 0, d = 1000L) {

  modelFit <- model[['finalModel']]
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
        stm2dns(newX[.x, ], memory.limit()),
        probability = TRUE
      )

      pb_all$tick(tokens = list(what = 'predictions'))

      set_names(as.integer(prediction) - 1L, names(prediction))
    }) %>%
      unlist  %>%
      factor(levels = c(0L, 1L), labels = (c('excluded', 'included'))) %>%
      set_names(str_replace(names(.), '^[0-9]+\\.', ''))
  )
}


tar_option_set(
  packages = c(
    "tidyverse", "caret", "unbalanced", "slam", "e1071", "Matrix", "tm",
    "pROC", "glmnet"
  ),
  error = "continue",
  workspace_on_error = TRUE,
  format = "qs" # quick save (faster than RDS, for internal targets)
)

# End this file with a list of target objects.
list(
  tar_file_read(
    summaries_pubmed, "data/summaries_pubmed.rda", read_from_path(!!.x)
  ),

  tar_target(
    corpus_train,
    create_train(summaries_pubmed[[1]]),
    iteration = "list",
    pattern = map(summaries_pubmed)
  ),


  tar_target(
    train_data,
    corpus_train |>
        lowering() |>
        nonword() |>
        stem(language = "english") |>
        white() |>
        ngram(),
    iteration = "list",
    pattern = map(corpus_train)
  ),

  tar_target(
    train_dtm,
    atom_dtm(train_data, parallel = FALSE),
    iteration = "list",
    pattern = map(train_data)
  ),


  tar_target(
    hutch_parameter,
    list(
      max_retained = (4/100), # 37589 * 2/10,
      seed          = 14,
      n_cv_folds    = 5,
      tuneLenght    = 10
    )
  ),

  tar_target(
    original_dtm,
    list(
      data   = train_dtm,
      labels = summaries_pubmed[[1]]$final |>
        factor(levels = c(0L, 1L), labels = c('excluded', 'included'))
      ),
    iteration = "list",
    pattern = map(train_dtm, summaries_pubmed)
  ),

  tar_target(
    data_used,
    variable_select(
      original_dtm[['data']],
      max_retained = as.integer(
        original_dtm[['data']]$ncol * hutch_parameter[['max_retained']]
      )
    ),
    iteration = "list",
    pattern = map(original_dtm)
  ),

  tar_target(
    mlts,
    {
      list(
        svm = svmLinear2_cvAble,
        # knn = knn_cvAble,
        # naive_bayes = nb_cvAble,
        # rf = rf_cvAble,
        # logitboost = LogitBoost_cvAble,
        glmnet = glmnet_cvAble
      )
    }
  ),
  tar_target(
    balanced_method,
    {
      list(
        none    = NULL,
        # ROS5050_new = ROS5050_new,
        RUS5050_new = RUS5050_new,
        # ROS3565_new = ROS3565_new,
        RUS3565_new = RUS3565_new
      )
    }
  ),

  tar_target(
    name,
    names(summaries_pubmed),
    pattern = map(summaries_pubmed)
  ),

  tar_target(
    out,
    caret::train(
      ## data
      x = as.data.frame(as.matrix(data_used)),
      y = original_dtm[['labels']],

      ## method
      method = mlts[[1]],

      ## metric
      metric = 'AUC',
      maximize = TRUE,

      ## tuning
      tuneLength = hutch_parameter$tuneLenght,

      ## control
      trControl = trainControl(
        method            = 'cv',
        number            = hutch_parameter$n_cv_folds,
        search            = 'random',
        savePredictions   = 'final',
        classProbs        = TRUE,
        selectionFunction = 'best',
        sampling          = balanced_method[[1]],
        summaryFunction   = summary_auc,
        seeds             = c(
          map(
            seq_len(hutch_parameter$n_cv_folds),
            ~rep.int(hutch_parameter$seed, hutch_parameter$tuneLenght)
          ),
          hutch_parameter$seed
        )
      )
    ),
    iteration = "list",
    pattern = cross(map(original_dtm, data_used), mlts, balanced_method)
  ),

  tar_target(
    cfm,
    caret::confusionMatrix(out, norm = 'none'),
    iteration = "list",
    pattern = map(out)
  ),

  tar_target(
    what,
    paste(
      name,
      str_extract(mlts[[1]][["label"]], '([^w\\(]|[- ])+'),
      names(balanced_method),
      sep = ' - '
    ),
    iteration = "list",
    pattern = cross(name, mlts, balanced_method)
  ),

  tar_target(
    plots,
    caret::plot.train(out, nameInStrip = TRUE, main = what),
    iteration = "list",
    pattern = map(out, what)
  ),

  tar_file_read(
    test_corpus_preproc,
    "data/test_corpus_preproc.rda",
    read_from_path(!!.x)
  ),
  tar_target(test_dtm, atom_dtm(test_corpus_preproc))
#   tar_target(
#     test_names,
#     names(test_corpus_preproc.rda)
#   ),



#   tar_target(
#     test_dtms,
#     test_dtm |>
#       dtm_lfilter(data_used) |>
#       reweights_test(data_used),
#     iteration = "list",
#     pattern = map(data_used)
#   ),
#
#   tar_target(
#     predictions,
#     make_prediction(out, test_dtm),
#     iteration = "list",
#     pattern = map(out, test_dtm)
#   ),
#   tar_target(
#     res_pubmed,
#     {
#       pubmed_files <- list.files("data-raw/raw_pubmed/")
#       order_res <- pubmed_files %>%
#         str_extract('[0-9]+') %>%
#         as.integer() %>%
#         order
#       pubmed_files <- pubmed_files[order_res]
#       purrr:::map(file.path("data-raw", "raw_pubmed", pubmed_files), ~ {
#         data <- readxl::read_xlsx(., sheet = 2) %>%
#           distinct(NCT, .keep_all = TRUE)
#         names(data) <- tolower(names(data))
#         data
#       })
#     }
#   ),
#
#
#   tar_target(
#     to_test_all,
#     (test_names %in% res_pubmed[['nct']]) |>
#       factor(levels = c(FALSE, TRUE), labels = c('excluded', 'included')) |>
#       set_names(test_names),
#     iteration = "list",
#     pattern = map(res_pubmed)
#   ),
#
#   tar_target(
#     n_true_all,
#     sum(to_test_all == 'included'),
#     pattern = map(to_test_all)
#   ),
#
#   tar_target(
#     cfm_pred_all,
#     caret::confusionMatrix(
#       data      = predictions,
#       reference = to_test_all[1:1000],
#       positive  = 'included'
#     ),
#     iteration = "list",
#     pattern = map(predictions, to_test_all)
#   )
)
