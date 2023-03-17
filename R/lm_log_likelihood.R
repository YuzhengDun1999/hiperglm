lm_log_likelihood = function(beta, design, outcome, noise_var = 1){
  residual = outcome - design %*% beta
  log_likelihood = -0.5 * log(noise_var) - t(residual) %*% residual / 2 / noise_var
  return(log_likelihood)
}

lm_log_likelihood_gradient = function(beta, design, outcome, noise_var = 1){
  return(-(t(design) %*% design %*% beta - t(design) %*% outcome) / noise_var)
}
