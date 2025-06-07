quadrat_patterns <- function(patterns){
  pat <- lapply(patterns, function(pp) {
        quads <- spatstat.geom::quadrats(pp, nx = 3, ny = 3)
        spatstat.geom::split.ppp(pp, f = quads)
      })
  return(pat)
}
