quadrat_patterns <- lapply(patterns, function(pp) {
        quads <- quadrats(pp, nx = 3, ny = 3)   # splitter i 3 x 3 vindue
        split.ppp(pp, f = quads)
      })
