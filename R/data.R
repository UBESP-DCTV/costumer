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
#'    \item{\code{abstract}}{character} Abstract of the record retrieved from
#'      pubmed.
#' }
#'
#' @source \url{https://www.ncbi.nlm.nih.gov/pubmed/24639059}
"liu_4h28"


#' Liu's Corpus
#'
#' This is the \code{\link[tm]{VCorpus}} representation of
#'\code{\link{liu_4h28}}
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
#' @source \url{https://www.ncbi.nlm.nih.gov/pubmed/24639059}
"liu_corpus"


#' Liu's Document-Term Matrix
#'
#' This is the Document-Term Matrix (with simple term frequency weights)
#' representation of \code{\link{liu_4h28}}
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
#' @source \url{https://www.ncbi.nlm.nih.gov/pubmed/24639059}
"liu_dtm"




# #' ClinicalTrials.gov snapshot
# #'
# #' 419411 record retrieved from ClinicalTrials.gov snapshot taken at January
# #' 5th, 2017.
# #'
# #' @format A data frame with 419411 rows and 6 variables:
# #'
# #' \describe{
# #'    \item{\code{final}}{integer} gold standard classification: 1 is positive
# #'      0 is negative.
# #'    \item{\code{id}}{integer} ID code as retrieved from pubmed.
# #'    \item{\code{title}}{character} Title of the record retrieved from pubmed.
# #'    \item{\code{year}}{integer} Year of the record retrieved from pubmed.
# #'    \item{\code{authors}}{character} Authors of the record retrieved from
# #'      pubmed.
# #'    \item{\code{abstract}}{character} Abstract of the record retrieved from
# #'      pubmed.
# #' }
# #'
# "summaries_ct"
#
#
#
#
# #' PubMed training data from 14 studies
# #'
# #' Positive PubMed records from the 14 studies reported in _Impact of
# #' searching clinical trials registries in Systematic Reviews of
# #' pharmaceutical treatments: methodological Systematic Review and
# #' reanalysis of meta-analyses_.
# #'
# #' @format A list of 14 data-frames with 418 (yang), 1675 (kourbeti), 209
# #'   (li), 414 (cavender), 418 (chatterjee), 1043 (funakoshi), 209 (meng),
# #'   413 (segelov), 206 (li), 412 (lv), 832 (wang), 209 (zhou), 623 (liu)
# #'   and 413 (douxfils) rows and 6 variables each one:
# #'
# #' \describe{
# #'    \item{\code{final}}{integer} gold standard classification: 1 is positive
# #'      0 is negative.
# #'    \item{\code{id}}{integer} ID code as retrieved from pubmed.
# #'    \item{\code{title}}{character} Title of the record retrieved from pubmed.
# #'    \item{\code{year}}{integer} Year of the record retrieved from pubmed.
# #'    \item{\code{authors}}{character} Authors of the record retrieved from
# #'      pubmed.
# #'    \item{\code{abstract}}{character} Abstract of the record retrieved from
# #'      pubmed.
# #' }
# #'
# #' @source \url{https://www.bmj.com/content/356/bmj.j448}
# "summaries_pubmed"



#' ROS3565_new
#'
#' Random Oversampling wit 35:65 ratio function to be used with \pkg{caret}
#'
#'
#' @format list of 3:
#'
#' \describe{
#'    \item{\code{name}}{chr} the name of the function.
#'    \item{\code{func}}{function} the function itself.
#'    \item{\code{first}}{lgl} flag to decide if it happens before or after
#'           preprocessing.
#' }
#'
"ROS3565_new"


#' ROS5050_new
#'
#' Random Oversampling wit 50:50 ratio function to be used with \pkg{caret}
#'
#'
#' @format list of 3:
#'
#' \describe{
#'    \item{\code{name}}{chr} the name of the function.
#'    \item{\code{func}}{function} the function itself.
#'    \item{\code{first}}{lgl} flag to decide if it happens before or after
#'           preprocessing.
#' }
#'
"ROS5050_new"


#' RUS3565_new
#'
#' Random Undersampling wit 35:65 ratio function to be used with \pkg{caret}
#'
#'
#' @format list of 3:
#'
#' \describe{
#'    \item{\code{name}}{chr} the name of the function.
#'    \item{\code{func}}{function} the function itself.
#'    \item{\code{first}}{lgl} flag to decide if it happens before or after
#'           preprocessing.
#' }
#'
"RUS3565_new"


#' RUS5050_new
#'
#' Random Oversampling wit 50:50 ratio function to be used with \pkg{caret}
#'
#'
#' @format list of 3:
#'
#' \describe{
#'    \item{\code{name}}{chr} the name of the function.
#'    \item{\code{func}}{function} the function itself.
#'    \item{\code{first}}{lgl} flag to decide if it happens before or after
#'           preprocessing.
#' }
#'
"RUS5050_new"


#' ROS3565_new
#'
#' Random Oversampling wit 35:65 ratio function to be used with \pkg{caret}
#'
#'
#' @format list of 3:
#'
#' \describe{
#'    \item{\code{name}}{chr} the name of the function.
#'    \item{\code{func}}{function} the function itself.
#'    \item{\code{first}}{lgl} flag to decide if it happens before or after
#'           preprocessing.
#' }
#'
"ROS3565_new"


#' svmLinear2_cvAble
#'
#' svmLinear2 method for \pkg{caret} able to correctly manage
#' crossvalidation when text mining, and in particular when iDF reweighting
#' occurs.
#'
#'
#' @format list of 13
#'
#' @source \url{https://topepo.github.io/caret/using-your-own-model-in-train.html}
"svmLinear2_cvAble"



#' nb_cvAble
#'
#' naive bayes method for \pkg{caret} able to correctly manage
#' crossvalidation when text mining, and in particular when iDF reweighting
#' occurs.
#'
#'
#' @format list of 13
#'
#' @source \url{https://topepo.github.io/caret/using-your-own-model-in-train.html}
"nb_cvAble"


#' knn_cvAble
#'
#' k-nearest neighbour method for \pkg{caret} able to correctly manage
#' crossvalidation when text mining, and in particular when iDF reweighting
#' occurs.
#'
#'
#' @format list of 13
#'
#' @source \url{https://topepo.github.io/caret/using-your-own-model-in-train.html}
"knn_cvAble"



#' rf_cvAble
#'
#' random forest method for \pkg{caret} able to correctly manage
#' crossvalidation when text mining, and in particular when iDF reweighting
#' occurs.
#'
#'
#' @format list of 15
#'
#' @source \url{https://topepo.github.io/caret/using-your-own-model-in-train.html}
"rf_cvAble"


#' LogitBoost_cvAble
#'
#' logitr boost method for \pkg{caret} able to correctly manage
#' crossvalidation when text mining, and in particular when iDF reweighting
#' occurs.
#'
#'
#' @format list of 13
#'
#' @source \url{https://topepo.github.io/caret/using-your-own-model-in-train.html}
"LogitBoost_cvAble"



#' glmnet_cvAble
#'
#' GLM-network method for \pkg{caret} able to correctly manage
#' crossvalidation when text mining, and in particular when iDF reweighting
#' occurs.
#'
#'
#' @format list of 15
#'
#' @source \url{https://topepo.github.io/caret/using-your-own-model-in-train.html}
"glmnet_cvAble"





