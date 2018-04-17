#' Liu's metanalysis positive trials and random negative ones
#'
#' 23 Clinical Trials retrieved from pubmed records with respect to the
#' metanalysis by Liu,
#' plus 400 negative sample (taken as the first ones ordered with the "best
#' match" criteria on PubMED) use as negative set for training.
#'
#' @format A data frame with 423 rows and 6 variables:
#'
#' \describe{
#'    \item{\code{final}}{integer} gold standard classification: 1 is positive
#'      0 is negative.
#'    \item{\code{id}}{integer} ID code as retrieved from pubmed.
#'    \item{\code{title}}{character} Title of the record retrieved from pubmed.
#'    \item{\code{year}}{integer} Year of the record retrieved from pubmed.
#'    \item{\code{authors}}{character} Authors of the record retrieved from
#'      pubmed.
#'    \item{\code{Abstract}}{character} Abstract of the record retrieved from
#'      pubmed.
#' }
#'
#' @source \link{https://www.ncbi.nlm.nih.gov/pubmed/24639059}
"liu_4h28"


#' Liu's Corpus
#'
#' This is the \code{\link[tm]{VCorpus}} representation of
#'\code{\link[hutch.code]{liu_4h28}}
#'
#'
#' @format A \code{[tm]{VCorpus}}
#'
#' \describe{
#'    \item{\code{content}}{named list} the text relative to each documents
#'    \item{\code{meta}}{list} the metadata relative to each documents
#'    \item{\code{dmeta}}{data.frame} the metadata relative to the corpora with the
#'          document relative classification
#' }
#'
#' @source \link{https://www.ncbi.nlm.nih.gov/pubmed/24639059}
"liu_corpus"


#' Liu's Document-Term Matrix
#'
#' This is the Document-Term Matrix (with simple term frequency weights)
#' representation of \code{\link[hutch.code]{liu_4h28}}
#'
#'
#' @format Both \code{[tm]{DocumentTermMatrix}} and
#'         \code{[slam]{simple_triplet_matrix}}
#'
#' \describe{
#'    \item{\code{i}}{integer vector} the document-wise (rows) coordinates of
#'      non-zero entries of the matrix, i.e. the lenght of i is the number of
#'      non-zero entries.
#'    \item{\code{j}}{integer vector} the term-wise (columns) coordinates of
#'      non-zero entries of the matrix, i.e. the lenght of j is the number of
#'      non-zero entries.
#'    \item{\code{v}}{numerical vector} the non-zero entries
#'      (frequencies/weight of the matrix) corresponding to the relative
#'      coordinates (i, j), i.e. the lenght of v is the number of non-zero
#'      entries.
#'    \item{\code{nrow}}{integer} the whole number of rows of the matrix
#'    \item{\code{ncol}}{integer} the whole number of columns of the matrix
#'    \item{\code{dimnames}}{list} a list of two character vectors representing
#'      the Terms/Tokens (the first one) and the name/ID of the documents (the
#'      second one)
#' }
#'
#' @source \link{https://www.ncbi.nlm.nih.gov/pubmed/24639059}
"liu_dtm"

