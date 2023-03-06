are_all_close <- function(v, w, abs_tol = 1e-6, rel_tol = 1e-6) {
  abs_diff <- abs(v - w)
  are_all_within_atol <- all(abs_diff < abs_tol)
  are_all_within_rtol <- all(abs_diff < rel_tol * pmax(abs(v), abs(w)))
  return(are_all_within_atol && are_all_within_rtol)
}

logit_newton = function(design, outcome, maxite = 1000){
  n_pred = ncol(design)
  beta_old = rep(0, n_pred)
  for (i in 1:maxite) {
    hessian = logit_log_likelihood_hessian(beta_old, design, outcome)
    gradient = logit_log_likelihood_gradient(beta_old, design, outcome)
    beta_new = beta_old - solve(hessian, gradient)
    loglik_old = logit_log_likelihood(beta_old, design, outcome)
    loglik_new = logit_log_likelihood(beta_new, design, outcome)
    if (are_all_close(loglik_old, loglik_new, n_pred / 100, n_pred / 100)) {
      break
    }
    else{
      beta_old = beta_new
    }
    if (i == maxite) {
      warning("Newton's method did not converge in ", maxite, " iterations.")
    }
  }
  return(beta_new)
}
