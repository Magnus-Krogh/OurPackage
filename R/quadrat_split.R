quadrat_patterns <- lapply(patterns, function(pp) {
        quads <- quadrats(pp, nx = 3, ny = 3)
        split.ppp(pp, f = quads)
      })
