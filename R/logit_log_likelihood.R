sigmoid = function(x){
  return(1 / (1 + exp(-x)))
}

logit_log_likelihood = function(beta, design, outcome){
  prob_1 = sigmoid(design %*% beta)
  log_likelihood = sum(outcome * log(prob_1) + (1 - outcome) * log(1 - prob_1))
  return(log_likelihood)
}

logit_log_likelihood_gradient = function(beta, design, outcome){
  prob_1 = sigmoid(design %*% beta)
  gradient = t(design) %*% (outcome - prob_1)
  return(gradient)
}

