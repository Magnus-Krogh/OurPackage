% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yule_walker_est.R
\name{plot}
\alias{plot}
\title{Yule Walker estimates from covariances}
\usage{
plot(x)
}
\arguments{
\item{x}{Vector of the variance and the covariance of the m first lags}

\item{n}{Sample size of the simulated AR(m) process}
}
\value{
A vector consisting of estimates of the AR parameter estimates as
well as the estimated variance of the white noise process. The vector has
attribute 'realization', which is a simulated time series using the estimates
as parameters of size n and a class denoted ar_mod.
}
\description{
Given covariances of the first m lags, computes the Yule-Walker estimates
in an AR(m) process.
}
