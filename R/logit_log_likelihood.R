sigmoid = function(x){
  return(1 / (1 + exp(-x)))
}

logit_log_likelihood = function(beta, design, outcome){
  pi = sigmoid(design %*% beta)
  log_likelihood = sum(outcome * log(pi) + (1 - outcome) * log(1 - pi))
  return(log_likelihood)
}

logit_log_likelihood_gradient = function(beta, design, outcome){
  pi = sigmoid(design %*% beta)
  gradient = t(design) %*% (outcome - pi)
  return(gradient)
}

logit_log_likelihood_hessian = function(beta, design, outcome){
  pi = sigmoid(design %*% beta)
  hessian = -t(design) %*% diag(pi[,1] * (1 - pi[,1])) %*% design
  return(hessian)
}
