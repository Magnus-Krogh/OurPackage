#' Splits a point pattern into quadrats
#'
#' @param patterns A list of point patterns
#'
#' @returns A list consisting of lists of the split point patterns
#' @export
#'
#' @examples
#' pats <- list(spatstat.random::rpoispp(lambda = 200),
#'           spatstat.random::rpoispp(lambda = 100))
#' split_pats <- quadrat_patterns(pats)
quadrat_patterns <- function(patterns){
  pat <- lapply(patterns, function(pp) {
        quads <- spatstat.geom::quadrats(pp, nx = 3, ny = 3)
        spatstat.geom::split.ppp(pp, f = quads)
      })
  return(pat)
}
