
# quadrat_patterns test
example1 <- list(spatstat.geom::ppp(x = c(0.3, 1.2, 2.7, 0.6, 1.4, 2.1, 0.95, 1.05, 2.5),
                                    y = c(0.5, 0.4, 0.1, 1.7, 1.85, 1.4, 2.7, 2.9, 2.4),
                                    window = spatstat.geom::owin(xrange = c(0,3), yrange = c(0,3))))
example1_split <- quadrat_patterns(example1)
expect_equal(current = list(example1_split[[1]][7]$`Tile row 3, col 1`,
                            example1_split[[1]][8]$`Tile row 3, col 2`,
                            example1_split[[1]][9]$`Tile row 3, col 3`,
                            example1_split[[1]][4]$`Tile row 2, col 1`,
                            example1_split[[1]][5]$`Tile row 2, col 2`,
                            example1_split[[1]][6]$`Tile row 2, col 3`,
                            example1_split[[1]][1]$`Tile row 1, col 1`,
                            example1_split[[1]][2]$`Tile row 1, col 2`,
                            example1_split[[1]][3]$`Tile row 1, col 3`),
             target = list(
  spatstat.geom::ppp(x = c(0.3), y = c(0.5), window = spatstat.geom::owin(xrange = c(0,1), yrange = c(0,1))),
  spatstat.geom::ppp(x = c(1.2), y = c(0.4), window = spatstat.geom::owin(xrange = c(1,2), yrange = c(0,1))),
  spatstat.geom::ppp(x = c(2.7), y = c(0.1), window = spatstat.geom::owin(xrange = c(2,3), yrange = c(0,1))),
  spatstat.geom::ppp(x = c(0.6), y = c(1.7), window = spatstat.geom::owin(xrange = c(0,1), yrange = c(1,2))),
  spatstat.geom::ppp(x = c(1.4), y = c(1.85), window = spatstat.geom::owin(xrange = c(1,2), yrange = c(1,2))),
  spatstat.geom::ppp(x = c(2.1), y = c(1.4), window = spatstat.geom::owin(xrange = c(2,3), yrange = c(1,2))),
  spatstat.geom::ppp(x = c(0.95), y = c(2.7), window = spatstat.geom::owin(xrange = c(0,1), yrange = c(2,3))),
  spatstat.geom::ppp(x = c(1.05), y = c(2.9), window = spatstat.geom::owin(xrange = c(1,2), yrange = c(2,3))),
  spatstat.geom::ppp(x = c(2.5), y = c(2.4), window = spatstat.geom::owin(xrange = c(2,3), yrange = c(2,3)))
))

# partition_function_strauss test
set.seed(1)
partition1 <- partition_function_strauss(n = 10000, beta = 1.3, gamma = 0.6, r = 0.2)
expect_equal(current = partition1, target = 1.279557, tolerance = 0.001)

# yule_walker_est test
set.seed(1)
estimates1 <- yule_walker_est(x = c(3.68, 2.29, 1.85), n = 100)
expect_equal(current = estimates1$parameters, target = c(0.5050070, 0.1884603, 2.1748823), tolerance = 0.0001)
expect_equal(current = attr(estimates1, "realization")[1:10], target = c(
  2.38799492, 1.78087489, -0.01681907, -3.27461847, 0.00528446,
  -0.06359707, -0.05599353, 1.36364365,  1.89974415,  1.83523911), tolerance = 0.0001)
