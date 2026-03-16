test_that("compose is still exported (kept for backward compat, marked internal)", {
  # compose is exported but marked @keywords internal
  exports <- getNamespaceExports("compositional.mle")
  expect_true("compose" %in% exports)
})

test_that("is_solver_result is exported", {
  exports <- getNamespaceExports("compositional.mle")
  expect_true("is_solver_result" %in% exports)
})

test_that("grid_search rejects massive grids", {
  expect_error(
    grid_search(lower = rep(0, 10), upper = rep(1, 10), n = 10),
    "Grid would have"
  )
})

test_that("sim_anneal reports converged = FALSE when not fully cooled", {
  problem <- make_normal_problem()
  result <- sim_anneal(max_iter = 5, min_temp = 1e-10)(problem, c(4, 1.5))
  expect_false(is_converged(result))
})

test_that("mle_constraint default has is_default = TRUE", {
  c_default <- mle_constraint()
  expect_true(c_default$is_default)
  c_custom <- mle_constraint(support = function(theta) theta[1] > 0)
  expect_false(c_custom$is_default)
})
