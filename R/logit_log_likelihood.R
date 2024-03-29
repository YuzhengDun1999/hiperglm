sigmoid = function(x){
  return(1 / (1 + exp(-x)))
}

logit_log_likelihood = function(beta, design, outcome){
  prob = sigmoid(design %*% beta)
  log_likelihood = sum(outcome * log(prob) + (1 - outcome) * log(1 - prob))
  return(log_likelihood)
}

logit_log_likelihood_gradient = function(beta, design, outcome){
  prob = sigmoid(design %*% beta)
  gradient = t(design) %*% (outcome - prob)
  return(gradient)
}

logit_log_likelihood_hessian = function(beta, design, outcome){
  prob = sigmoid(design %*% beta)
  hessian = -t(design) %*% diag(prob[,1] * (1 - prob[,1])) %*% design
  return(hessian)
}
