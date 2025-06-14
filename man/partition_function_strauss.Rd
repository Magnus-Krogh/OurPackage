% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/partition_function_strauss.R
\name{partition_function_strauss}
\alias{partition_function_strauss}
\title{Approximates the partition function of a Strauss process on the unit window}
\usage{
partition_function_strauss(n, beta, gamma, r)
}
\arguments{
\item{n}{The number of simulated point patterns}

\item{beta}{Parameter in the model. Must be positive}

\item{gamma}{Parameter in the model. Must be greater than or equal to 0 and
less than or equal to 1.}

\item{r}{Parameter in the model. Must be positive}
}
\value{
An estimate of the partition function
}
\description{
Approximates the partition function of a Strauss process on the unit window
}
\examples{
set.seed(2)
partition_function_strauss(n = 1000, beta = 5, gamma = 0.75, r = 0.25)
#34.66769
}
