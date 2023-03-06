logit_bfgs = function(design, outcome){
  n_pred = ncol(design)
  beta0 = rep(0, n_pred)
  beta_optim = optim(beta0, logit_log_likelihood, gr = logit_log_likelihood_gradient,
                     design = design, outcome = outcome,
                     method = "BFGS", control=list(fnscale=-1))
  return(beta_optim$par)
}
