testthat::test_that("linalg and optim least-sq coincide", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = "linear", seed = 1918)
  design <- data$design; outcome <- data$outcome
  via_linalg_out <- hiper_glm(design, outcome, model = "linear")
  via_bfgs_out <- hiper_glm(
    design, outcome, model = "linear", option = list(mle_solver = "BFGS")
  )
  expect_true(are_all_close(
    coef(via_linalg_out), coef(via_bfgs_out), abs_tol = 1e-4, rel_tol = 1e-4
  ))
})

testthat::test_that("newton and bfgs outputs coincide on logit model", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'logit', seed = 1918)
  design <- data$design; outcome <- data$outcome
  via_newton_qr_out <- hiper_glm(design, outcome, model = 'logit', newton_opt = 'qr')
  via_newton_lu_out <- hiper_glm(design, outcome, model = 'logit', newton_opt = 'lu')
  via_newton_chol_out <- hiper_glm(design, outcome, model = 'logit', newton_opt = 'chol')
  via_bfgs_out <- hiper_glm(
    design, outcome, model = 'logit', option = list(mle_solver = 'BFGS')
  )
  expect_true(are_all_close(
    coef(via_newton_qr_out), coef(via_bfgs_out), abs_tol = 1e-3, rel_tol = 1e-3
  ))
  expect_true(are_all_close(
    coef(via_newton_lu_out), coef(via_bfgs_out), abs_tol = 1e-3, rel_tol = 1e-3
  ))
  expect_true(are_all_close(
    coef(via_newton_chol_out), coef(via_bfgs_out), abs_tol = 1e-3, rel_tol = 1e-3
  ))
})
