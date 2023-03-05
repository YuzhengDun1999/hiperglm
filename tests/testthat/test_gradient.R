test_that(
  "test for the gradient of log likelihood in linear model", {
    n_obs = 32; n_pred = 4
    data = simulate_data(n_obs, n_pred, model = 'linear', seed = 1918)
    design = data$design; outcome = data$outcome
    beta = c(3, 1, 4, 1)
    analytical_grad <- lm_log_likelihood_gradient(beta, design, outcome)
    numerical_grad <- approx_grad(function(beta) lm_log_likelihood(beta, design, outcome), beta)
    testthat::expect_true(
      are_all_close(analytical_grad, numerical_grad, abs_tol = 1e-3, rel_tol = 1e-3)
    )
  }
)
