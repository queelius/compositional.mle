test_that("solver_result has correct class vector", {
  problem <- make_normal_problem()
  result <- gradient_ascent(max_iter = 20)(problem, c(0, 1))
  expect_s3_class(result, "solver_result")
  expect_s3_class(result, "mle_fit_numerical")
  expect_s3_class(result, "mle_fit")
})

test_that("solver_info structure is always present", {
  problem <- make_normal_problem()
  result <- gradient_ascent(max_iter = 20)(problem, c(0, 1))
  expect_true(!is.null(result$solver_info))
  expect_type(result$solver_info$name, "character")
  expect_type(result$solver_info$iterations, "integer")
  expect_type(result$solver_info$converged, "logical")
  expect_null(result$solver_info$composition)
})

test_that("solver_info$name matches solver factory", {
  problem <- make_normal_problem()
  solvers <- list(
    gradient_ascent = gradient_ascent(max_iter = 10),
    newton_raphson = newton_raphson(max_iter = 10),
    bfgs = bfgs(max_iter = 10),
    nelder_mead = nelder_mead(max_iter = 10)
  )
  for (name in names(solvers)) {
    result <- solvers[[name]](problem, c(4, 1.5))
    expect_equal(result$solver_info$name, name)
  }
})

test_that("fisher_scoring reports correct solver name", {
  problem <- make_normal_problem(fisher = TRUE)
  result <- fisher_scoring(max_iter = 10)(problem, c(4, 1.5))
  expect_equal(result$solver_info$name, "fisher_scoring")
})

test_that("is_solver_result works", {
  problem <- make_normal_problem()
  result <- gradient_ascent(max_iter = 10)(problem, c(0, 1))
  expect_true(is_solver_result(result))
  expect_false(is_solver_result(list()))
  expect_false(is_solver_result(42))
})

test_that("algebraic.mle generics work on solver_result", {
  problem <- make_normal_problem()
  result <- bfgs()(problem, c(4, 1.5))
  expect_length(params(result), 2)
  expect_true(is.numeric(as.numeric(logLik(result))))
  expect_true(is.numeric(AIC(result)))
  expect_true(is.matrix(vcov(result)))
  expect_length(se(result), 2)
})

test_that("composition sets solver_info$composition", {
  problem <- make_normal_problem()
  strategy <- gradient_ascent(max_iter = 10) %>>% newton_raphson(max_iter = 5)
  result <- strategy(problem, c(0, 1))
  expect_equal(result$solver_info$composition$strategy, "sequential")
  expect_true(length(result$solver_info$composition$chain) >= 2)
})

test_that("race sets solver_info$composition", {
  problem <- make_normal_problem()
  strategy <- gradient_ascent(max_iter = 10) %|% nelder_mead(max_iter = 10)
  result <- strategy(problem, c(4, 1.5))
  expect_equal(result$solver_info$composition$strategy, "race")
  expect_true(length(result$solver_info$composition$alternatives) == 2)
})

test_that("diagnostics present for sim_anneal", {
  problem <- make_normal_problem()
  result <- sim_anneal(max_iter = 50)(problem, c(4, 1.5))
  expect_true(!is.null(result$solver_info$diagnostics$final_temp))
  expect_true(!is.null(result$solver_info$diagnostics$acceptance_rate))
})

test_that("diagnostics present for coordinate_ascent", {
  problem <- make_normal_problem()
  result <- coordinate_ascent(max_cycles = 5)(problem, c(4, 1.5))
  expect_true(!is.null(result$solver_info$diagnostics$cycles))
})

test_that("diagnostics present for grid_search", {
  problem <- make_normal_problem()
  result <- grid_search(lower = c(0, 0.5), upper = c(10, 4), n = 5)(problem, c(0, 1))
  expect_true(!is.null(result$solver_info$diagnostics$grid_points))
  expect_true(!is.null(result$solver_info$diagnostics$grid_evaluated))
})
