are_all_close <- function(v, w, abs_tol = 1e-6, rel_tol = 1e-6) {
  abs_diff <- abs(v - w)
  are_all_within_atol <- all(abs_diff < abs_tol)
  are_all_within_rtol <- all(abs_diff < rel_tol * pmax(abs(v), abs(w)))
  return(are_all_within_atol && are_all_within_rtol)
}

take_one_newton_step = function(design, outcome, beta_old, newton_opt = "qr"){
  hessian = logit_log_likelihood_hessian(beta_old, design, outcome)
  gradient = logit_log_likelihood_gradient(beta_old, design, outcome)
  supported_method <- c("qr", "lu", "chol")
  if (!(newton_opt %in% supported_method)) {
    stop("Inputed newton_opt is not supported, please use qr, lu or chol")
  }
  if (newton_opt == "lu") {
    beta_new = beta_old - solve(hessian, gradient)
  }
  else if (newton_opt == "chol") {
    L = chol(-hessian)
    beta_new = beta_old + backsolve(L, backsolve(L, gradient, transpose = TRUE))
  }
  else {
    prob = sigmoid(design %*% beta_old)
    weight_vec = logit_weight_vec(beta_old, design)
    y_adjusted = sqrt(weight_vec) * design %*% beta_old + 1 / sqrt(weight_vec) * (outcome - prob)
    x_adjusted = diag(sqrt(weight_vec)) %*% design
    beta_new = qr.solve(x_adjusted, y_adjusted)
  }
  return(beta_new)
}

logit_newton = function(design, outcome, maxiter = 1000, newton_opt = "qr"){
  n_pred = ncol(design)
  beta_old = rep(0, n_pred)
  for (i in 1:maxiter) {
    beta_new = take_one_newton_step(design, outcome, beta_old, newton_opt)
    loglik_old = logit_log_likelihood(beta_old, design, outcome)
    loglik_new = logit_log_likelihood(beta_new, design, outcome)
    if (are_all_close(loglik_old, loglik_new)) {
      break
    }
    else{
      beta_old = beta_new
    }
    if (i == maxiter) {
      warning("Newton's method did not converge in ", maxiter, " iterations.")
    }
  }
  return(beta_new)
}
