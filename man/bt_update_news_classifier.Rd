% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bt_update_news_classifier.R
\name{bt_update_news_classifier}
\alias{bt_update_news_classifier}
\title{Pulls news leads from the Ricardomain database and trains a new Mrs Hudson
model based on these leads, then saves it.}
\usage{
bt_update_news_classifier(
  db.connection = NULL,
  show.text.summary = T,
  create.training.testing.split = F,
  training.testing.split = 0.9,
  mrs.h.method = "d2v",
  mrs.h.gen.alg = "XGB",
  use.tf.idf = TRUE
)
}
\arguments{
\item{db.connection}{Database connection, should default to the correct one.}

\item{show.text.summary}{Do you want to see a summary of the input training
data wordcounts?}

\item{create.training.testing.split}{Do you want to test the model on part of
the training data and see its precision and recall?}

\item{training.testing.split}{Proportion to split for training and testing.}
}
\value{
Void. Returns a summary of the precision and recall of the model if
  create.training.testing.split=T
}
\description{
Pulls news leads from the Ricardomain database and trains a new Mrs Hudson
model based on these leads, then saves it.
}
\references{
www.globaltradealert.org
}
