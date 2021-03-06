% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data2corpus.R
\name{data2corpus}
\alias{data2corpus}
\title{Import to corpus}
\usage{
data2corpus(data_df, ids = NULL, suffix_name = NULL)
}
\arguments{
\item{data_df}{A data frame which is expected to have (at least) the six
columns final (integer 0/1 or factror), id, title, authors and
abstract (character vectors) and year (integer).}

\item{ids}{A character vector indicating the name (if any) of the data frame
reporting the ids of the documents. If not NULL (default) each
document in the final corpus is named with the corresponding IDs.}

\item{suffix_name}{Character vector representing the "name" of data (to be
used as suffix in IDs). Default is NULL.}
}
\value{
A \code{\link[tm]{VCorpus}} object in which documents are created
        merging title and abstract of a given data frame which have textual
        columns named title and abstract
}
\description{
This function create a corpus based on the kind of data we use as input, i.e.
a data frame with six column (i.e. final, id, title, year, authors and
abstract)
}
\examples{
data2corpus(liu_4h28)
data2corpus(liu_4h28, 'id')
data2corpus(liu_4h28, 'id', 'Liu')
}
