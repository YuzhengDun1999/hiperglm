#' @export
hiper_glm <- function(design, outcome, model, option = list()){
  supported_model <- c("linear", "logit")
  if (!(model %in% supported_model)) {
    stop(sprintf("The model %s is not supported.", model))
  }
  hglm_out <- list()
  class(hglm_out) <- "hglm"
  if(model == "linear"){
    if(is.null(option$mle_solver)){
      option$mle_solver = "PINV"
    }
    if(option$mle_solver == "PINV"){
      hglm_out$coef = lm_pseudo_inv(design, outcome)
    }
    else if(option$mle_solver == "BFGS"){
      hglm_out$coef = lm_bfgs(design, outcome)
    }
    else{
      stop(print("This mle algorithm in lnear model is not supported."))
    }
  }
  else if(model == "logit"){
    if(is.null(option$mle_solver)){
      option$mle_solver = "Newton"
    }
    if(option$mle_solver == "Newton"){
      hglm_out$coef = logit_newton(design, outcome)
    }
    else if(option$mle_solver == "BFGS"){
      hglm_out$coef = logit_bfgs(design, outcome)
    }
    else{
      stop(print("This mle algorithm in lnear model is not supported."))
    }
  }
  return(hglm_out)
}
