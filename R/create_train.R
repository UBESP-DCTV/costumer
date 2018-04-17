#' Create training data structure
#'
#' Training data use the \pkg{tm} data structure.
#'
#' @param data_df A data frame which is expected to have (at least) the six
#'        columns final (integer 0/1 or factror), id, title, authors and
#'        abstract (character vectors) and year (integer).
#'
#' @param name_df Character vector representing the "name" of data (to be used
#'        as suffix in IDs).
#'#'
#' @return A \code{\link[tm]{VCorpus}} data structure with metadata for
#'         classification, id, year and authors.
#' @export
#'
#' @examples
#' library(hutch.code)
#' create_train(liu_4h28)
#'
#' create_train(liu_4h28, 'Liu')

create_train <- function(data_df, name_df = NULL) {

  ## Check the correctness of the input
  #
  if (!inherits(data_df, 'data.frame')) stop('data_df has to be a data frame')

  if (!all(
        c('final', 'id', 'title', 'year', 'authors', 'abstract') %in%
        names(data_df)
      )
     ) stop('data_df has espected to have at least "final", "id", "title", "year", "authors" and "abstract" columns.')

  if (!inherits(data_df[['final']], c('integer', 'factor'))) {
    stop('class of the column "final" has to be integer or factor.')
  }

  if (!inherits(data_df[['year']], 'integer')) {
    stop('class of the column "year" has to be integer.')
  }

  if (
    !inherits(data_df[['id']], 'character') ||
    !inherits(data_df[['title']], 'character') ||
    !inherits(data_df[['authors']], 'character') ||
    !inherits(data_df[['abstract']], 'character')
  ) {
    stop('class of the columns "title", "authors" and "abstract" has to be character.')
  }

  ## Import in tm
  #
  pb <- progress::progress_bar$new(
    format = " create_train: :what [:bar] :percent in :elapsed",
    total  = 4,
    clear  = FALSE,
    width  = 60
  )
  tick <- function(x, pb, token = '', ...) {
    pb$tick(tokens = list(what = token), ...)
    invisible(x)
  }

  pb$tick(0, token = list(what = 'Corpus creation'))
  corpus <- data2corpus(data_df, 'id', name_df) %>%  # this authomatically set id metadata

    tick(pb, 'assigning authors') %>%
    v_assign_meta('author', data_df$authors) %>%
    # v_assign_meta('id', data_df$id) %>% # this is not to do beacause renaming
                                        # documents already update the "id"
                                        # filed of the corpus metadata

    tick(pb, 'assigning years') %>%
    v_assign_meta('year', data_df$year)

  pb$tick(token = list(what = 'Assigning labels'))
  corpus <- tm:::`meta<-.VCorpus`(corpus, 'real_label', value = data_df$final)

  pb$tick(token = list(what = 'End'))
  corpus
}

