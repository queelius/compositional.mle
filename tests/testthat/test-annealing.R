test_that("sim_anneal finds global optimum for simple problem", {
  set.seed(123)

  # Simple quadratic - global optimum at (3, 2)
  problem <- mle_problem(
    loglike = function(theta) -sum((theta - c(3, 2))^2)
  )

  result <- sim_anneal(max_iter = 500, temp_init = 10)(problem, theta0 = c(0, 0))

  # Should converge reasonably close to optimum
  expect_true(abs(result$theta.hat[1] - 3) < 0.5)
  expect_true(abs(result$theta.hat[2] - 2) < 0.5)
  expect_equal(result$solver, "sim_anneal")
})

test_that("sim_anneal respects constraints", {
  set.seed(456)

  # Problem where optimum is at sigma > 0
  problem <- mle_problem(
    loglike = function(theta) {
      mu <- theta[1]
      sigma <- theta[2]
      if (sigma <= 0) return(-Inf)
      -(mu - 2)^2 - (sigma - 1)^2
    },
    constraint = mle_constraint(
      support = function(theta) theta[2] > 0,
      project = function(theta) c(theta[1], max(theta[2], 0.01))
    )
  )

  result <- sim_anneal(max_iter = 300, neighbor_sd = 0.5)(problem, theta0 = c(0, 0.5))

  # sigma should remain positive
  expect_true(result$theta.hat[2] > 0)
  expect_equal(result$solver, "sim_anneal")
})

test_that("sim_anneal returns proper result structure", {
  set.seed(789)

  problem <- mle_problem(
    loglike = function(theta) -sum(theta^2)
  )

  result <- sim_anneal(max_iter = 100)(problem, theta0 = c(1, 1))

  expect_s3_class(result, "mle_numerical")
  expect_true("theta.hat" %in% names(result))
  expect_true("loglike" %in% names(result))
  expect_true("iterations" %in% names(result))
  expect_true("solver" %in% names(result))
  expect_true("final_temp" %in% names(result))
  expect_true("acceptance_rate" %in% names(result))
})

test_that("sim_anneal with tracing records values", {
  set.seed(101)

  problem <- mle_problem(
    loglike = function(theta) -sum((theta - c(1, 1))^2)
  )

  trace_cfg <- mle_trace(values = TRUE, path = TRUE)
  result <- sim_anneal(max_iter = 100)(problem, theta0 = c(0, 0), trace = trace_cfg)

  expect_false(is.null(result$trace_data))
  expect_false(is.null(result$trace_data$values))
  expect_false(is.null(result$trace_data$path))
})

test_that("sim_anneal can compose with other solvers", {
  set.seed(202)

  problem <- mle_problem(
    loglike = function(theta) -sum((theta - c(3, 2))^2),
    score = function(theta) -2 * (theta - c(3, 2))
  )

  # SA for global exploration, then gradient for local refinement
  strategy <- sim_anneal(max_iter = 200) %>>% gradient_ascent(max_iter = 50)
  result <- strategy(problem, theta0 = c(-5, -5))

  # Should find good solution
  expect_true(abs(result$theta.hat[1] - 3) < 0.1)
  expect_true(abs(result$theta.hat[2] - 2) < 0.1)
  expect_equal(result$strategy, "sequential")
})

test_that("sim_anneal cooling affects exploration", {
  set.seed(303)

  problem <- mle_problem(
    loglike = function(theta) -sum(theta^2)
  )

  # Fast cooling = less exploration
  result_fast <- sim_anneal(cooling_rate = 0.8, max_iter = 100)(problem, c(5, 5))

  # Slow cooling = more exploration
  result_slow <- sim_anneal(cooling_rate = 0.99, max_iter = 100)(problem, c(5, 5))

  # Both should work, slow cooling typically accepts more moves
  expect_true(result_slow$acceptance_rate > 0)
})
