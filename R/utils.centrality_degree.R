centrality_degree <- function(G, type="total") {
  G <- as.matrix(G)
  if (type=="total") {
    c <- as.numeric((G + t(G)) %*% rep(1,nrow(G)))
  } else if (type=="in") {
    c <- as.numeric(t(G) %*% rep(1,nrow(G)))
  } else if (type=="out") {
    c <- as.numeric(G %*% rep(1,nrow(G)))
  }
  return(c)
}