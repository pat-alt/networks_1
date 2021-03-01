katz_bonacich <- function(alpha, beta, G) {
  d <- nrow(G)
  qr.solve(diag(d) - beta * G, alpha * rep(1,d))
}