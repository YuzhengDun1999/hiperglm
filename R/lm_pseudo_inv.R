lm_pseudo_inv = function(design, outcome){
  return(qr.solve(t(design) %*% design, t(design) %*% outcome))
}
