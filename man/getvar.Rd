% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Fcreate_Prerequisites.R
\name{getvar}
\alias{getvar}
\title{Creates a column by implementing the operation performed in 'term'}
\usage{
getvar(df, term)
}
\arguments{
\item{df}{A data frame with columns given in term parameter}

\item{term}{A string which has the operation to be performed uisng two columns of a given data frame}
}
\value{
A data vector derived by the given operation.
}
\description{
Takes in a data frame and an operation to be performed on columns of the dataframe and return a data vector after implementing the given operation
}
