#' @export
hiper_glm <- function(design, outcome, model, option = list()){
  supported_model <- c("linear", "logit")
  if (!(model %in% supported_model)) {
    stop(sprintf("The model %s is not supported.", model))
  }
  hglm_out <- list()
  class(hglm_out) <- "hglm"
  if (model == "linear") {
    if ((is.null(option$mle_solver)) || (option$mle_solver == "qr")) {
      hglm_out$coef = lm_qr(design, outcome)
    }
    else if (option$mle_solver == "BFGS") {
      hglm_out$coef = bfgs(design, outcome, lm_log_likelihood, lm_log_likelihood_gradient)
    }
    else {
      stop("Inputed method is not supported, please use Newton or BFGS")
    }
  }
  else if (model == "logit") {
    if ((is.null(option$mle_solver)) || (option$mle_solver == "Newton")) {
      hglm_out$coef = logit_newton(design, outcome)
    }
    else if (option$mle_solver == "BFGS") {
      hglm_out$coef = bfgs(design, outcome, logit_log_likelihood, logit_log_likelihood_gradient)
    }
    else {
      stop("Inputed method is not supported, please use Newton or BFGS")
    }
  }
  return(hglm_out)
}
