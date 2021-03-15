degree_dist <- function(G, plot=TRUE) {
  d <- diag(crossprod(G))
  degree_dist <- table(d)/length(d)
  if (plot) {
    barplot(degree_dist)
  }
  return(degree_dist)
}