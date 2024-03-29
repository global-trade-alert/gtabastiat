% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bt_d2v_col_preprocess.R
\name{bt_d2v_col_preprocess}
\alias{bt_d2v_col_preprocess}
\title{Prepare a doc2vec matrix using a pretrained word2vec model.}
\usage{
bt_d2v_col_preprocess(d2v.col, doc_id, text, as.df = T)
}
\arguments{
\item{doc_id}{the doc ids, probably the BID with Bastiat}

\item{text}{character vector of words separated by spaces}

\item{as.df}{return as a dataframe? if F, returns a matrix}

\item{model.w2v}{pretrained word embeddings}
}
\description{
To be used on text data from a particular feature column (e.g. title, description or AA)
}
