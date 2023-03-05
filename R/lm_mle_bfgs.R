lm_log_likelihood = function(beta, design, outcome, noise_var = 1){
  residual = outcome - design %*% beta
  log_likelihood = -0.5 * log(noise_var) - t(residual) %*% residual / 2 / noise_var
  return(log_likelihood)
}

lm_log_likelihood_gradient = function(beta, design, outcome, noise_var = 1){
  return(-(t(design) %*% design %*% beta - t(design) %*% outcome) / noise_var)
}

lm_bfgs = function(design, outcome, noise_var = 1){
  n_pred = ncol(design)
  beta0 = rep(0, n_pred)
  beta_optim = optim(beta0, lm_log_likelihood, gr = lm_log_likelihood_gradient,
                     design = design, outcome = outcome,
                     method = "BFGS", control=list(fnscale=-1))
  return(beta_optim$par)
}
