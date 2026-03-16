## Tests for gradient ascent solver

test_that("gradient_ascent works on simple quadratic problem", {
  problem <- mle_problem(
    loglike = function(theta) -(theta[1]^2 + theta[2]^2),
    score = function(theta) -2 * theta
  )

  solver <- gradient_ascent(max_iter = 100, line_search = TRUE)
  result <- solver(problem, c(2, 2))

  expect_s3_class(result, "solver_result")
  expect_true(abs(result$theta.hat[1]) < 0.1)
  expect_true(abs(result$theta.hat[2]) < 0.1)
})

test_that("gradient_ascent respects max_iter", {
  problem <- mle_problem(
    loglike = function(theta) -(theta[1]^2 + theta[2]^2),
    score = function(theta) -2 * theta
  )

  solver <- gradient_ascent(learning_rate = 0.001, max_iter = 5, line_search = FALSE)
  result <- solver(problem, c(10, 10))

  expect_equal(result$solver_info$iterations, 5L)
  expect_false(result$converged)
})

test_that("gradient_ascent works with constrained optimization", {
  constraint <- mle_constraint(
    support = function(theta) all(theta > 0),
    project = function(theta) pmax(theta, 0.01)
  )

  problem <- mle_problem(
    loglike = function(theta) -(theta[1]^2 + theta[2]^2),
    score = function(theta) -2 * theta,
    constraint = constraint
  )

  solver <- gradient_ascent(max_iter = 100, line_search = TRUE)
  result <- solver(problem, c(2, 2))

  # Should converge near boundary
  expect_true(all(result$theta.hat > 0))
  expect_true(all(result$theta.hat < 0.2))
})

test_that("gradient_ascent works on 1D problems", {
  problem <- mle_problem(
    loglike = function(theta) -(theta - 5)^2,
    score = function(theta) -2 * (theta - 5)
  )

  solver <- gradient_ascent(max_iter = 100, line_search = TRUE)
  result <- solver(problem, 0)

  expect_true(abs(result$theta.hat - 5) < 0.5)
})

test_that("gradient_ascent without analytic score uses numerical", {
  problem <- mle_problem(
    loglike = function(theta) -(theta[1]^2 + theta[2]^2)
    # No score provided - will use numerical gradient
  )

  solver <- gradient_ascent(max_iter = 100, line_search = TRUE)
  result <- solver(problem, c(2, 2))

  expect_true(abs(result$theta.hat[1]) < 0.5)
  expect_true(abs(result$theta.hat[2]) < 0.5)
})
