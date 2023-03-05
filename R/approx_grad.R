approx_grad <- function(func, x, dx = .Machine$double.eps^(1/3)) {
  numerical_grad <- rep(0, length(x))
  for (i in 1:length(x)) {
    x_plus_dx = x
    x_plus_dx[i] = x_plus_dx[i] + dx
    numerical_grad[i] = (func(x_plus_dx) - func(x)) / dx
  }
  return(numerical_grad)
}
