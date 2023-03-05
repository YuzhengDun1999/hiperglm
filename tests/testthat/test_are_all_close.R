testthat::test_that(
  "Return True when two vectors are close", {
    x = c(1, 2, 3, 4)
    y = x + 1e-8
    testthat::expect_true(
      are_all_close(x, y)
    )
  }
)

testthat::test_that(
  "Return False when relative error is above rel_tol", {
    x = c(1, 2, 3, 4)
    y = x + 1e-6
    testthat::expect_false(
      are_all_close(x, y, abs_tol = 1e-5, rel_tol = 1e-9)
    )
  }
)

testthat::test_that(
  "Return False when absolute error is above abs_tol", {
    x = c(1, 2, 3, 4)
    y = x + 1e-7
    testthat::expect_false(
      are_all_close(x, y, abs_tol = 1e-9, rel_tol = 1e-3)
    )
  }
)
