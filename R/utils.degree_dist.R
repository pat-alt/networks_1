degree_dist <- function(G, plot=TRUE, mode="in") {
  if (mode=="in") {
    d <- t(G)%*%rep(1,nrow(G))
  } else {
    d <- G%*%rep(1,nrow(G))
  }
  degree_dist <- table(d)/length(d)
  if (plot) {
    barplot(degree_dist)
  }
  return(degree_dist)
}