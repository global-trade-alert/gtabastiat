% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bt_td_matrix_preprocess.R
\name{bt_td_matrix_preprocess}
\alias{bt_td_matrix_preprocess}
\title{Preprocess a document corpus into fixed-length vectors of integers, returned
as a data.frame or matrix. Error thrown if you don't have dedicated nVidia
GPU, this can be ignored.}
\usage{
bt_td_matrix_preprocess(
  num_words = 15000,
  max_length = 100,
  text,
  tokeniser = NULL,
  as.df = T
)
}
\arguments{
\item{num_words}{Desired size of vocabulary.}

\item{max_length}{Desired length of each doc. Shorter will be chopped. Longer
will be zero-padded.}

\item{text}{The document corpus.}

\item{as.df}{Do you want a dataframe? If false, a matrix is returned.}
}
\value{
sparse document TD matrix, as a dataframe, or matrix if as.df=F
}
\description{
Requires Keras to work.
}
\references{
www.globaltradealert.org
}
