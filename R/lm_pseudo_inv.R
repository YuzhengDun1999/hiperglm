lm_pseudo_inv = function(design, outcome){
  return(solve(t(design) %*% design, t(design) %*% outcome))
}
