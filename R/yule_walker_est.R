#' Given covariances of the first m lags, computes the Yule-Walker estimates
#' in an AR(m) process.
#'
#' @param x Vector of the variance and the covariance of the m first lags
#' @param n Sample size of the simulated AR(m) process
#'
#' @returns A vector consisting of estimates of the AR parameters estimates as
#' well as the estimated variance of the white noise process. The vector has
#' attribute 'realization', which is a simulated time series using the estimates
#' as parameters of size n and a class denoted ar_mod.
#' @export
plot <- function(x){
  UseMethod("plot")
}
#' @export
plot.ar_mod <- function(x){
  graphics::plot.default(attr(x, "realization"), type ="l")
}
#' @export
print <- function(x){
  UseMethod("print")
}
#' @export
print.ar_mod <- function(x){
  print.default(x$parameters)
}
#'
#'
#'
#' @examples
#' estimates <- yule_walker_est(c(4,3,2,1), n = 100)
#'
yule_walker_est <- function(x, n){
  number_of_params <- length(x) - 1
  ps <- c(x[2]/x[1])
  v <- c(x[1]*(1-ps[1]^2))
  if (number_of_params > 1){
    for (i in 2:number_of_params){
      temp_phis <- rep(0, i-1)
      sum <- 0
      for (j in 1:(i-1)){
        sum <- sum + ps[j] * x[i - j + 1]
      }
      phi_p <- (x[i + 1] - sum) / v
      for (j in 1:(i-1)){
        temp_phis[j] <- ps[j] - phi_p * ps[i - j]
      }
      ps <- c(temp_phis, phi_p)
      v <- v*(1 - ps[i]^2)
    }
  }
  sum <- 0
  for (i in 1:(number_of_params)){
    sum <- sum + ps[i]*x[i+1]
  }
  x <- list(parameters = c(ps, x[1] - sum))
  attr(x, "realization") <-  stats::arima.sim(list(ar = x$parameters[1:(length(x)-1)]), n = n, sd = sqrt(x$parameters[length(x)]))[1:n]
  class(x) <- "ar_mod"
  return(x)
}
