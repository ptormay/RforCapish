% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/df.R
\name{df}
\alias{df}
\title{Return the dataframe}
\usage{
df(KMdata)
}
\arguments{
\item{KMdata}{a data frame for calculating survival curve}
}
\value{
a data frame containing the result of the survival calculation
}
\description{
Takes in a data frame or csv file consisting of four columns:
 - Patient Key
 - category
 - event parameter (censoring)
 - time
}

