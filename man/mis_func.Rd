% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Fcreate_Prerequisites.R
\name{mis_func}
\alias{mis_func}
\title{Checks the null values (NA, blanks and Infinite values) in a gven data vector}
\usage{
mis_func(data_col)
}
\arguments{
\item{data_col}{Any data vector in which the missing data is to be checked}
}
\value{
A boolean value returning TRUE if more than 30% of the data is null and FALSE otherwise
}
\description{
Makes a check on the given data vector if it has a minimum 70% data without NULL values
}
