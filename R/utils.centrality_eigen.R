centrality_eigen <- function(G, scale=TRUE) {
  list2env(eigen(G), envir = environment())
  v <- abs(vectors[,1]) # by Perron-Forbenius Theorem
  if (scale) {
    v <- v/max(v)
  }
  return(v)
}