#' Import to corpus
#'
#' This function create a corpus based on the kind of data we use as input, i.e.
#' a data frame with six column (i.e. final, id, title, year, authors and
#' abstract)
#'
#' @param data_df A data frame which is expected to have (at least) the six
#'        columns final (integer 0/1 or factror), id, title, authors and
#'        abstract (character vectors) and year (integer).
#'
#' @param ids A character vector indicating the name (if any) of the data frame
#'        reporting the ids of the documents. If not NULL (default) each
#'        document in the final corpus is named with the corresponding IDs.
#'
#' @param suffix_name Character vector representing the "name" of data (to be
#'        used as suffix in IDs). Default is NULL.
#'
#' @return A \code{\link[tm]{VCorpus}} object in which documents are created
#'         merging title and abstract of a given data frame which have textual
#'         columns named title and abstract
#' @export
#'
#' @examples
#' library(hutch.code)
#' data2corpus(liu_4h28)
#'
#' data2corpus(liu_4h28, 'id')
#'
#' data2corpus(liu_4h28, 'id', 'Liu')
data2corpus <- function(data_df, ids = NULL, suffix_name = NULL) {
  if (!inherits(data_df, 'data.frame')) stop('data_df has to be a data frame')

  if (!all(c('title', 'abstract') %in% names(data_df))
  ) stop('data_df has espected to have at least "title", and "abstract" columns.')

  if (
    !inherits(data_df[['title']], 'character') ||
    !inherits(data_df[['abstract']], 'character')
  ) stop('class of the columns "title", and "abstract" has to be character.')

  if (!is.null(ids) && !ids %in% names(data_df)) stop('ids has to be a name of data_df\'s column')

  ## Import in tm --------------------------------------------------------------
  #
  pb <- progress::progress_bar$new(
    format = " data2corpus: :what [:bar] :percent in :elapsed",
    total  = 5,
    clear  = FALSE,
    width  = 60
  )
  tick <- function(x, pb, token = '', ...) {
    pb$tick(tokens = list(what = token), ...)
    invisible(x)
  }

  pb$tick(0, tokens = list(what = 'Setup'))
  corpus <- data_df %>%

    tick(pb, 'Pasting Tit-Ab') %>%
    dplyr::transmute(
      doc_id = id,
      text   = paste(title, abstract)
    ) %>%

    tick(pb, 'Sourcing') %>%
    tm::DataframeSource() %>%

    tick(pb, 'VCorpus') %>%
    tm::VCorpus()


  pb$tick(tokens = list(what = 'Naming'))
  if(!is.null(ids)) {
    suffix_name <- ifelse(!is.null(suffix_name), paste0(suffix_name, '_'), '')
    names(corpus) <- paste0(suffix_name, data_df[[ids]])
  }
  names(corpus$content) <- names(corpus)


  pb$tick(tokens = list(what = 'End'))
  corpus
}
