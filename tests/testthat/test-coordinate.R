test_that("coordinate_ascent finds optimum for simple problem", {
  # Simple quadratic - optimum at (3, 2)
  problem <- mle_problem(
    loglike = function(theta) -sum((theta - c(3, 2))^2),
    constraint = mle_constraint(support = function(theta) TRUE)
  )

  result <- coordinate_ascent(max_cycles = 30)(problem, theta0 = c(0, 0))

  # Should converge to optimum
  expect_true(abs(result$theta.hat[1] - 3) < 0.1)
  expect_true(abs(result$theta.hat[2] - 2) < 0.1)
  expect_equal(result$solver, "coordinate_ascent")
})

test_that("coordinate_ascent respects constraints", {
  # Problem with positivity constraint on second parameter
  problem <- mle_problem(
    loglike = function(theta) {
      if (theta[2] <= 0) return(-Inf)
      -(theta[1] - 2)^2 - (theta[2] - 1)^2
    },
    constraint = mle_constraint(
      support = function(theta) theta[2] > 0,
      project = function(theta) c(theta[1], max(theta[2], 0.01))
    )
  )

  result <- coordinate_ascent(max_cycles = 20)(problem, theta0 = c(0, 0.5))

  # Second parameter should remain positive
  expect_true(result$theta.hat[2] > 0)
})

test_that("coordinate_ascent converges with convergence flag", {
  problem <- mle_problem(
    loglike = function(theta) -sum((theta - c(1, 1))^2),
    constraint = mle_constraint(support = function(theta) TRUE)
  )

  result <- coordinate_ascent(max_cycles = 50, tol = 1e-6)(problem, theta0 = c(0, 0))

  # Should converge
  expect_true(result$converged)
  expect_equal(result$solver, "coordinate_ascent")
})

test_that("coordinate_ascent returns proper result structure", {
  problem <- mle_problem(
    loglike = function(theta) -sum(theta^2),
    constraint = mle_constraint(support = function(theta) TRUE)
  )

  result <- coordinate_ascent(max_cycles = 10)(problem, theta0 = c(1, 1))

  expect_s3_class(result, "mle_numerical")
  expect_true("theta.hat" %in% names(result))
  expect_true("loglike" %in% names(result))
  expect_true("iterations" %in% names(result))
  expect_true("solver" %in% names(result))
  expect_true("cycles" %in% names(result))
})

test_that("coordinate_ascent with tracing records values", {
  problem <- mle_problem(
    loglike = function(theta) -sum((theta - c(1, 1))^2),
    constraint = mle_constraint(support = function(theta) TRUE)
  )

  trace_cfg <- mle_trace(values = TRUE, path = TRUE)
  result <- coordinate_ascent(max_cycles = 10)(problem, theta0 = c(0, 0), trace = trace_cfg)

  expect_false(is.null(result$trace_data))
  expect_true(!is.null(result$trace_data$values))
  expect_true(!is.null(result$trace_data$path))
})

test_that("coordinate_ascent random cycle order works", {
  set.seed(123)

  problem <- mle_problem(
    loglike = function(theta) -sum((theta - c(2, 3))^2),
    constraint = mle_constraint(support = function(theta) TRUE)
  )

  result <- coordinate_ascent(max_cycles = 20, cycle_order = "random")(
    problem, theta0 = c(0, 0)
  )

  # Should still converge
  expect_true(abs(result$theta.hat[1] - 2) < 0.1)
  expect_true(abs(result$theta.hat[2] - 3) < 0.1)
})

test_that("coordinate_ascent without line_search works", {
  problem <- mle_problem(
    loglike = function(theta) -sum((theta - c(1, 1))^2),
    constraint = mle_constraint(support = function(theta) TRUE)
  )

  result <- coordinate_ascent(max_cycles = 30, line_search = FALSE)(
    problem, theta0 = c(0, 0)
  )

  # Should make progress (may not converge as precisely without line search)
  expect_true(result$loglike > -2)  # Better than starting point
})

test_that("coordinate_ascent can compose with other solvers", {
  problem <- mle_problem(
    loglike = function(theta) -sum((theta - c(3, 2))^2),
    score = function(theta) -2 * (theta - c(3, 2)),
    constraint = mle_constraint(support = function(theta) TRUE)
  )

  # Coordinate ascent followed by Newton refinement
  strategy <- coordinate_ascent(max_cycles = 10) %>>% gradient_ascent(max_iter = 20)
  result <- strategy(problem, theta0 = c(0, 0))

  # Should find optimum
  expect_true(abs(result$theta.hat[1] - 3) < 0.01)
  expect_true(abs(result$theta.hat[2] - 2) < 0.01)
  expect_equal(result$strategy, "sequential")
})
