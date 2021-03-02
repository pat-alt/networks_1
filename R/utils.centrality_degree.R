centrality_degree <- function(G) {
  G <- as.matrix(G)
  c <- c(G %*% rep(1,nrow(G)) + t(G) %*% rep(1,nrow(G)))
  return(c)
}