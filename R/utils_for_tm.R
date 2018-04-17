#===============================================================================
#' Assign a vector of metadata to a corpus
#'
#' This function provide the possibility to assign a vector of metadata (one
#' each document) to a copus in one command.
#' To assign a single metadata to a single document or to the whole corpuse
#' use the standard \code{\link[tm]{meta}}.
#'
#' @param corpus A VCorpus
#' @param meta_name A character vector representing the name of meta to consider
#' @param meta_content A vector or a list of data of length of corpus containing
#'        the corresponding meta data content to assign each one
#'
#' @return A VCorpus (invisible) with metadata updated
#' @export
#'
#' @examples
#' library(hutch.code)
#' data(liu_4h28)
#'
#' corpus <- create_train(data_df = liu_4h28, name_df = 'Liu')
#' corpus <- assign_meta(corpus, 'author', liu_4h28$authors)
v_assign_meta <- function(corpus, meta_name, meta_content) {
  ## Check the correctness of the input ----------------------------------------
  #
  if (!inherits(corpus, 'VCorpus')) {
    stop('Sorry, for the moment this function works only for VCorpus')
  }
  if (!is.character(meta_name)) stop('meta_name has to be a character vector')
  if (length(meta_content) != length(corpus)) {
    stop('Content as to be of the same lenght of the corpus')
  }

  ## Behind the scene a VCorps is a list of 3 element (take a look at what
  ## is returned by the function `VCorpus()` (run `VCorpus` and look at the last
  ## line), and what it is: run `tm:::as.VCorpus.list`). So, we want to modify
  ## its content`. map2 scan two list in parallel (`.x` and `.y`) applying the
  ## defined function (at the right of the `~`) to each corresponding couple of
  ## elements.
  ## The content of a VCorpus is a list of document, and each one of them is a
  ## list of two: its content and its meta (take a look at `DataframeSource`
  ## which call `SimpleSource()` to define the reader which, by default, is
  ## `readPlain` which call `PlainTextDocument()` returning that list). So,
  ## we want to modify an element ("author") of the meta of each document.
  ## At the end, we have to return the modified element (the document with
  ## the updated "author" meta).
  #
  corpus$content <- purrr::map2(
    .x = NLP::content(corpus), .y = meta_content,
    ~ {
      .x$meta[[meta_name]] <- .y
      .x
    }
  )

  invisible(corpus)
}


#===============================================================================
#' Transform on HUTCH corpora
#'
#' Interface to apply (using \code{\pkg{purrr}}) transformation functions (also
#' denoted as mappings) to corpora. This is a slightly modification of the
#' \code{link[tm]{tm_map}} function.
#'
#' @param corpus A VCorpus
#' @param FUN a transformation function taking a text document (a character
#'            vector) as input and returning a text document (a character vector
#'            of the same length as the input vector).
#' @param ... arguments to FUN
#'
#' @return A corpus with FUN applied to each document in x.
#' @export
#'
#' @examples
#' data(liu_4h28)
#'
#' corpus <- data2corpus(liu_4h28, 'id', 'Liu')
#' hutch_map(corpus, tolower)
hutch_map <- function(corpus, FUN, ...) {

  ## Input check
  #
  if (!inherits(corpus, 'VCorpus')) stop('corpus has to be a VCorpus')

  ## main function
  #
  corpus$content <- purrr::map(NLP::content(corpus), FUN, ...)
  corpus
}


#===============================================================================
#' Corpora into list and back to corpora
#'
#' These functions are useful to create a list of documents from a
#' \code{\link[tm]{VCorpus}} object (\code{corpora2list}) saving all the
#' information to came back to the original structure (\code{list2corpora}).
#'
#' The aims is to provide a suitable input to function optimizated to
#' used native list objects such as \code{\link{list_to_dtm}} to manage the
#' content of the documents, and next to come back to the original structure
#' of \code{\link[tm]{VCorpus}}.
#'
#' @describeIn corpora2list Create a \code{corpora_list} object from a corpora.
#' @param corpora A \code{\link[tm]{Vcorpus}}
#'
#' @return \code{corpora2list} returns a List of documents with metadata stored
#'         as attributes, i.e. document-specific metadata are stored in a list
#'         attribute named "metadata" of length equal of the lenght of the
#'         corpora; the dmeta corpora-specific metadata are stored in an
#'         attribute named "dmeta".
#' @export
#'
#' @examples
#'
#' data(liu_4h28)
#' corpora  <- data2corpora(liu_4h28)
#' doc_list <- corpora2list(corpora)
corpora2list <- function(corpora) {

  if (!inherits(corpora, 'VCorpus')) stop('corpora must be a VCorpus')

  documents <- purrr::map(corpora$content, ~.$content)
  names(documents) <- names(corpora)

  metadata  <- purrr::map(corpora$content, ~.$meta)
  dmeta     <- tm:::meta.VCorpus(corpora)

  attr(documents, 'metadata') <- metadata
  attr(documents, 'dmeta')    <- dmeta

  class(documents) <- c(class(documents), 'corpora_list')

  documents
}


#' @describeIn corpora2list Recreate a corpora from a \code{corpora_list}
#'             object.
#'
#' @param corpora_list A \code{corpora_list} object
#' @return \code{list2corpora} returns a \code{\link[tm]{VCorpus}}.
#' @export
#'
#' @examples
#'
#' corpora_back <- list2corpora(doc_list)
list2corpora <- function(corpora_list) {

  if (!inherits(corpora_list, 'corpora_list'))
    stop('This function is intended to be used only for "corpora_list" object.')

  corpora <- tm::VCorpus(tm::VectorSource(corpora_list))
  names(corpora) <- names(corpora_list)

  purrr::walk(.x = attr(corpora_list, 'metadata')[[1]] %>% names(),
    ~ {
      ## For each kind of metadata we need to retrieve the list of all of them
      #
      metas  <- purrr::map(.x = seq_along(corpora_list),
        function(actual_doc) attr(corpora_list, 'metadata')[[actual_doc]][[.x]]
      )
      corpora <<- v_assign_meta(corpora, .x, metas)
    }
  )

  corpora$dmeta <- attr(corpora_list, 'dmeta')

  corpora
}


