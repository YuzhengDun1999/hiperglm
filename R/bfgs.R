bfgs = function(design, outcome, log_likelihood, gradient,...){
  n_pred = ncol(design)
  beta0 = rep(0, n_pred)
  beta_optim = optim(beta0, log_likelihood, gr = gradient,
                     design = design, outcome = outcome,
                     method = "BFGS", control=list(fnscale=-1), ...)
  return(beta_optim$par)
}
