cluster_coeff <- function(G, type="individual") {
  G <- as.matrix(G)
  deg <- G %*% rep(1,ncol(G))
  norm <- (1/(deg*(deg-1)))
  norm[is.infinite(norm)] <- 0
  clust <- as.numeric(norm * (G * crossprod(G)) %*% rep(1,ncol(G)))
  if (type=="average") {
    clust <- mean(clust_coeffs)
  } 
  return(clust)
}
