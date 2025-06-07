#' Approximates the partition function of a Strauss process on the unit window
#'
#' @param n The number of simulated point patterns
#' @param beta Parameter in the model. Must be positive
#' @param gamma Parameter in the model. Must be greater than or equal to 0 and
#' less than or equal to 1.
#' @param r Parameter in the model. Must be positive
#'
#' @returns An estimate of the partition function
#' @export
#'
#' @examples
#' set.seed(2)
#' partition_function_strauss(n = 1000, beta = 5, gamma = 0.75, r = 0.25)
#' #34.66769
partition_function_strauss <- function(n, beta, gamma, r){
  if (beta > 0 && gamma >= 0 && gamma <= 1 && r > 0){
    sum <- 0
    for (i in 1:n){
      X <- spatstat.random::rpoispp(lambda = 1)
      antal <- spatstat.geom::npoints(X)
      if (antal > 1){
        count <- 0
        for (k in 1:antal){
          for (j in 1:antal){
            if (k != j){
              if (sqrt((X$x[k]-X$x[j])^2+(X$y[k]-X$y[j])^2)<=r){
              count <- count+1
              }
            }
          }
        }
        sum <- sum+beta^(antal)*gamma^(count)
      }
      else{
        sum <- sum + beta^(antal)
      }
    }
    return(sum/n)
  }
  else{
    stop("Parameters do not satisfy the correct restrictions")
  }
}
