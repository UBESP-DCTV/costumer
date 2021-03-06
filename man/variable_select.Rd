% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variable_select.R
\name{variable_select}
\alias{variable_select}
\title{Variable selector (best tfidf rank)}
\usage{
variable_select(original_train, max_retained = min(1000L,
  as.integer(ncol(original_train) * 10/100)))
}
\arguments{
\item{original_train}{original (\code{\link[slam]{simple_triplet_matrix}})
with overall variables}

\item{max_retained}{the maximum number of variable to retain
(default is
\code{min(1000L, as.integer(ncol(original) * 10/100))})}
}
\value{
an object of the same class of \code{original} with only the
        top tifidf max_retained token in the sum of relative tfidf
}
\description{
Variable selector (best tfidf rank)
}
\examples{
data(liu_dtm)
variable_select(liu_dtm)
}
