# Packages =================================================================
options(java.parameters = "-Xmx128g")    # set java to use up to 128 GiB-RAM

library(costumer)
library(stringr)
library(pryr)
library(log4r)
library(magrittr)
library(progress)
library(devtools)
library(parallel)
n_cores <- max(1, detectCores() - 1)
cl      <- makeCluster(n_cores)

# Utilities ================================================================

# progressbar --------------------------------------------------------------
#
pb <- progress::progress_bar$new(
  format = " ct_corpus_and_dtm: :what [:bar] :percent in :elapsed",
  total  = 12,
  clear  = FALSE,
  width  = 60
)
tick <- function(x, pb, token = '', ...) {
  pb$tick(tokens = list(what = token), ...)
  invisible(x)
}

# logger -------------------------------------------------------------------
#
pathlog    <- file.path(getwd(), 'log')
costumer:::loggeRs(prj_name = 'hutch', pathlog = pathlog)


# Main code ================================================================

log4r::info(logger.1, 'ct_corpus_and_dtm - start')
log4r::info(logger.1, capture.output(devtools::session_info()))

# load data ----------------------------------------------------------------
#
data("summaries_ct")

# create corpus ------------------------------------------------------------
#
log4r::info(logger.1, 'test_corpus - start')
pb$tick(0, token = list(what = 'test_corpus'))

ct_corpus_time <- system.time(
  ct_corpus_mem <- pryr::mem_change({
    #
    test_corpus <- create_train(summaries_ct)
    log4r::info(logger.1, 'test_corpus ready')
    #
  })
)

log4r::info(logger.1, stringr::str_c(
  'test_corpus created in ', ct_corpus_time, ' of time and ', ct_corpus_mem,
  ' of RAM.'
))

pb$tick(token = list(what = 'saving test_corpus'))
devtools::use_data(test_corpus, overwrite = TRUE, compress = 'xz')

# preprocess corpus --------------------------------------------------------
#
log4r::info(logger.1, 'test_corpus_preproc - start')
pb$tick(token = list(what = 'preproc'))

preproc_time <- system.time(
  preproc_mem <- pryr::mem_change({
    #
    test_corpus_preproc <- test_corpus %>%

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
    #
  })
)

log4r::info(logger.1, stringr::str_c(
  'test_corpus preprocessed in ', preproc_time, ' of time and ',
  preproc_mem, ' of RAM.'
))

pb$tick(token = list(what = 'saving test_corpus_preproc'))
devtools::use_data(test_corpus_preproc, overwrite = TRUE, compress = 'xz')

# dtm creation -------------------------------------------------------------
#
log4r::info(logger.1, 'test_dtm - start')
pb$tick(token = list(what = 'test_dtm'))

dtm_time <- system.time({
  #
  test_dtm <- atom_dtm(test_corpus_preproc, step = 1000L, parallel = FALSE)
  log4r::info(logger.1, 'test_dtm ready')
  #
})

log4r::info(logger.1, stringr::str_c(
  'test_dtm preprocessed in ', dtm_time,
  ' of time (RAM used NA because of parallel computation)'
))

pb$tick(token = list(what = 'saving test_dtm'))
devtools::use_data(test_dtm, overwrite = TRUE, compress = 'xz')

pb$tick(token = list(what = 'end'))

