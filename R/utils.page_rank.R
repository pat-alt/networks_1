page_rank <- function(transition_mat, tol=1e-9, max_iter=100) {
  T_ <- transition_mat
  T_temp <- transition_mat
  tol <- 1e-9
  k <- 2
  converged <- FALSE
  while(!converged & k<max_iter) {
    T_new <- T_%^%k
    converged <- all(abs(T_temp - T_new) < tol)
    T_temp <- T_new
    k <- k + 1
  }
  r <- diag(T_temp)
  return(
    list(
      r = r,
      k = k,
      tol = tol
    )
  )
}