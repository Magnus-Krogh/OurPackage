funk <- function(n, beta, gamma, r){
  if (beta < 0 && gamma >= 0 && gamma <= 1 & r >= 0){
    sum = 0
    for (i in 1:n){
      X = spatstat.random::rpoispp(1, spatstat.geom::owin())
      antal = spatstat.geom::npoints(X)
      if (antal > 1){
        count = 0
        for (k in 1:antal){
          for (j in 1:antal){
            if (i != j){
              if (sqrt((X$x[i]-X$x[j])^2-(X$y[i]-X$y[j])^2)<=r){
              count = count+1
              }
            }
          }
        }
      }
      sum = sum+beta^(antal)*gamma^(count)
    }
    return(sum/n)
  }
  else{
    print("Parameters do not satisfy the correct restrictions")
  }
}
