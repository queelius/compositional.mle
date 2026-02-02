## Integration tests for complete MLE workflows

test_that("Normal distribution MLE with gradient ascent", {
  problem <- make_normal_problem(seed = 123, fisher = TRUE)

  solver <- gradient_ascent(max_iter = 200)
  result <- solver(problem, c(0, 1))

  # Check parameter estimates are reasonable
  expect_true(abs(result$theta.hat[1] - 5) < 0.5)
  expect_true(abs(result$theta.hat[2] - 2) < 0.5)
})

test_that("Normal distribution MLE with Newton-Raphson", {
  problem <- make_normal_problem(seed = 456, true_mu = 10, true_sigma = 3, fisher = TRUE)

  solver <- newton_raphson(max_iter = 50)
  result <- solver(problem, c(5, 1))

  # Newton-Raphson should find good estimates
  expect_true(abs(result$theta.hat[1] - 10) < 0.5)
  expect_true(abs(result$theta.hat[2] - 3) < 0.5)
})

test_that("Grid search finds approximate solution", {
  problem <- mle_problem(
    loglike = function(theta) -(theta[1] - 2)^2 - (theta[2] + 1)^2
  )

  solver <- grid_search(lower = c(-5, -5), upper = c(5, 5), n = 10)
  result <- solver(problem, c(0, 0))

  expect_s3_class(result, "mle_grid_search")
  expect_true(abs(result$theta.hat[1] - 2) < 1.5)
  expect_true(abs(result$theta.hat[2] - (-1)) < 1.5)
})

test_that("Sequential composition works", {
  problem <- make_normal_problem(seed = 123, fisher = TRUE)

  # Grid search then gradient ascent
  solver <- grid_search(c(0, 0.5), c(10, 4), n = 5) %>>% gradient_ascent(max_iter = 100)
  result <- solver(problem, c(0, 1))

  expect_true(is_mle_numerical(result))
  expect_true(abs(result$theta.hat[1] - 5) < 0.5)
  expect_true(abs(result$theta.hat[2] - 2) < 0.5)
})

test_that("with_restarts improves robustness", {
  problem <- make_normal_problem(seed = 123, fisher = TRUE)

  sampler <- uniform_sampler(c(-5, 0.5), c(15, 5))
  solver <- with_restarts(gradient_ascent(max_iter = 100), n = 5, sampler = sampler)
  result <- solver(problem, c(0, 1))

  expect_true(is_mle_numerical(result))
  expect_true(abs(result$theta.hat[1] - 5) < 0.5)
  expect_true(abs(result$theta.hat[2] - 2) < 0.5)
})

test_that("Constrained optimization respects bounds", {
  # Constrain to [0, 1] x [0, 1]
  constraint <- mle_constraint(
    support = function(theta) all(theta >= 0 & theta <= 1),
    project = function(theta) pmax(0, pmin(1, theta))
  )

  problem <- mle_problem(
    loglike = function(theta) -(theta[1] - 3)^2 - (theta[2] - 3)^2,
    score = function(theta) c(-2 * (theta[1] - 3), -2 * (theta[2] - 3)),
    constraint = constraint
  )

  solver <- gradient_ascent(learning_rate = 0.1, max_iter = 100, line_search = FALSE)
  result <- solver(problem, c(0.5, 0.5))

  # Should converge to corner (1, 1)
  expect_true(abs(result$theta.hat[1] - 1) < 0.1)
  expect_true(abs(result$theta.hat[2] - 1) < 0.1)
})

test_that("BFGS solver works on simple problem", {
  # Simple quadratic problem without constraints
  problem <- mle_problem(
    loglike = function(theta) -(theta[1] - 2)^2 - (theta[2] + 1)^2
  )

  solver <- bfgs(max_iter = 100)
  result <- solver(problem, c(0, 0))

  expect_true(is_mle_numerical(result))
  expect_true(abs(result$theta.hat[1] - 2) < 0.5)
  expect_true(abs(result$theta.hat[2] - (-1)) < 0.5)
})

test_that("Nelder-Mead solver works", {
  problem <- mle_problem(
    loglike = function(theta) -(theta[1] - 2)^2 - (theta[2] + 1)^2
  )

  solver <- nelder_mead(max_iter = 500)
  result <- solver(problem, c(0, 0))

  expect_true(is_mle_numerical(result))
  expect_true(abs(result$theta.hat[1] - 2) < 0.5)
  expect_true(abs(result$theta.hat[2] - (-1)) < 0.5)
})

test_that("L-BFGS-B respects bounds", {
  problem <- mle_problem(
    loglike = function(theta) -(theta[1] - 5)^2 - (theta[2] - 5)^2
  )

  solver <- lbfgsb(lower = c(0, 0), upper = c(2, 2))
  result <- solver(problem, c(1, 1))

  expect_true(is_mle_numerical(result))
  expect_true(abs(result$theta.hat[1] - 2) < 0.1)
  expect_true(abs(result$theta.hat[2] - 2) < 0.1)
})

test_that("unless_converged applies refinement when needed", {
  problem <- mle_problem(
    loglike = function(theta) -(theta[1]^2 + theta[2]^2),
    score = function(theta) -2 * theta
  )

  # unless_converged takes two solver arguments
  solver <- unless_converged(
    gradient_ascent(max_iter = 100),
    newton_raphson(max_iter = 100)
  )
  result <- solver(problem, c(1, 1))

  expect_true(is_mle_numerical(result))
})

test_that("Parallel race selects best result", {
  problem <- mle_problem(
    loglike = function(theta) -(theta[1] - 3)^2 - (theta[2] + 2)^2
  )

  solver <- nelder_mead(max_iter = 100) %|% bfgs(max_iter = 50)
  result <- solver(problem, c(0, 0))

  expect_true(is_mle_numerical(result))
  expect_true(abs(result$theta.hat[1] - 3) < 0.5)
  expect_true(abs(result$theta.hat[2] - (-2)) < 0.5)
})
