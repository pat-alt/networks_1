erdos_renyi <- function(n=100,p=0.02) {
  g_ij <- rbinom((n*(n-1)/2),c(0,1),prob = p)
  G <- matrix(rep(0,n^2),n)
  G[lower.tri(G)] <- g_ij
  G <- G+t(G)
  return(G)
}