## Grid Search Tests

test_that("grid_search works without refinement", {
  problem <- mle_problem(
    loglike = function(theta) -(theta[1]^2 + theta[2]^2)
  )

  solver <- grid_search(lower = c(-2, -2), upper = c(2, 2), n = 5)
  result <- solver(problem, c(0, 0))

  expect_s3_class(result, "mle_grid_search")
  expect_length(result$theta.hat, 2)
  expect_true(all(result$theta.hat >= -2 & result$theta.hat <= 2))
  expect_true(abs(result$theta.hat[1]) <= 1)  # Should be near 0
  expect_true(abs(result$theta.hat[2]) <= 1)
})

test_that("grid_search validates inputs", {
  # Mismatched dimensions
  expect_error(
    grid_search(lower = c(-1, -1), upper = c(1, 1, 1), n = 5)
  )

  # Invalid bounds
  expect_error(
    grid_search(lower = c(1, 1), upper = c(-1, -1), n = 5)
  )
})

test_that("grid_search with 1D problem", {
  problem <- mle_problem(
    loglike = function(theta) -(theta - 3)^2
  )

  solver <- grid_search(lower = 0, upper = 6, n = 10)
  result <- solver(problem, 0)

  expect_length(result$theta.hat, 1)
  expect_true(abs(result$theta.hat - 3) < 1)
})

test_that("grid_search records metadata", {
  problem <- mle_problem(
    loglike = function(theta) -sum(theta^2)
  )

  solver <- grid_search(lower = c(-1, -1), upper = c(1, 1), n = 3)
  result <- solver(problem, c(0, 0))

  expect_false(is.null(result$grid_points))
  expect_false(is.null(result$grid_evaluated))
})

## Random Search Tests

test_that("random_search basic functionality", {
  problem <- mle_problem(
    loglike = function(theta) -sum(theta^2)
  )

  sampler <- uniform_sampler(c(-5, -5), c(5, 5))
  solver <- random_search(sampler, n = 100)
  result <- solver(problem, c(0, 0))

  expect_s3_class(result, "mle_random_search")
  expect_false(is.null(result$n_samples))
  expect_false(is.null(result$n_evaluated))
})

test_that("random_search validates inputs", {
  # Invalid sampler
  expect_error(random_search("not a function", n = 100))

  # Invalid n
  expect_error(random_search(function() runif(2), n = 0))
})

test_that("random_search finds reasonable solution", {
  problem <- mle_problem(
    loglike = function(theta) -(theta[1] - 3)^2 - (theta[2] + 2)^2
  )

  sampler <- uniform_sampler(c(-10, -10), c(10, 10))
  solver <- random_search(sampler, n = 1000)
  result <- solver(problem, c(0, 0))

  # Should be in the right ballpark
  expect_true(abs(result$theta.hat[1] - 3) < 3)
  expect_true(abs(result$theta.hat[2] - (-2)) < 3)
})

## with_restarts Tests

test_that("with_restarts basic functionality", {
  problem <- mle_problem(
    loglike = function(theta) -sum(theta^2),
    score = function(theta) -2 * theta
  )

  sampler <- uniform_sampler(c(-5, -5), c(5, 5))
  solver <- with_restarts(gradient_ascent(max_iter = 50), n = 5, sampler = sampler)
  result <- solver(problem, c(0, 0))

  expect_true(is_mle_numerical(result))
  expect_false(is.null(result$n_restarts))
})

test_that("with_restarts respects problem constraints", {
  constraint <- mle_constraint(
    support = function(theta) all(theta > 0),
    project = function(theta) pmax(theta, 1e-6)
  )

  problem <- mle_problem(
    loglike = function(theta) {
      if (any(theta <= 0)) return(-Inf)
      -sum((theta - 1)^2)  # optimum at (1, 1)
    },
    score = function(theta) -2 * (theta - 1),
    constraint = constraint
  )

  # Sampler produces some negative values, but constraint should handle it
  sampler <- uniform_sampler(c(-2, -2), c(5, 5))
  solver <- with_restarts(gradient_ascent(max_iter = 50), n = 10, sampler = sampler)
  result <- solver(problem, c(1, 1))

  expect_true(is_mle_numerical(result))
  expect_true(all(result$theta.hat > 0))
})

test_that("with_restarts stores best solution", {
  problem <- mle_problem(
    loglike = function(theta) -(theta[1] - 3)^2 - (theta[2] + 2)^2,
    score = function(theta) c(-2 * (theta[1] - 3), -2 * (theta[2] + 2))
  )

  sampler <- uniform_sampler(c(-10, -10), c(10, 10))
  solver <- with_restarts(gradient_ascent(max_iter = 100), n = 10, sampler = sampler)
  result <- solver(problem, c(0, 0))

  # Should find optimum near (3, -2)
  expect_true(abs(result$theta.hat[1] - 3) < 1)
  expect_true(abs(result$theta.hat[2] - (-2)) < 1)
})

## Composition Tests

test_that("sequential composition %>>% works", {
  problem <- mle_problem(
    loglike = function(theta) -(theta[1] - 5)^2 - (theta[2] - 5)^2
  )

  solver <- grid_search(c(-10, -10), c(10, 10), n = 5) %>>%
            nelder_mead(max_iter = 100)
  result <- solver(problem, c(0, 0))

  expect_true(is_mle_numerical(result))
  expect_true(abs(result$theta.hat[1] - 5) < 0.5)
  expect_true(abs(result$theta.hat[2] - 5) < 0.5)
})

test_that("three-stage composition works", {
  problem <- mle_problem(
    loglike = function(theta) -(theta[1]^2 + theta[2]^2),
    score = function(theta) -2 * theta
  )

  solver <- grid_search(c(-5, -5), c(5, 5), n = 3) %>>%
            gradient_ascent(max_iter = 50) %>>%
            nelder_mead(max_iter = 50)
  result <- solver(problem, c(0, 0))

  expect_true(is_mle_numerical(result))
  expect_true(abs(result$theta.hat[1]) < 0.5)
  expect_true(abs(result$theta.hat[2]) < 0.5)
})

test_that("unless_converged only runs refinement on failure", {
  problem <- mle_problem(
    loglike = function(theta) -(theta[1]^2 + theta[2]^2),
    score = function(theta) -2 * theta
  )

  # unless_converged takes two solvers
  solver <- unless_converged(
    gradient_ascent(max_iter = 100),
    gradient_ascent(max_iter = 1000)
  )
  result <- solver(problem, c(1, 1))

  expect_true(is_mle_numerical(result))
  expect_true(result$converged)
})

test_that("parallel race %|% works", {
  problem <- mle_problem(
    loglike = function(theta) -(theta[1] - 3)^2 - (theta[2] + 1)^2
  )

  solver <- nelder_mead(max_iter = 100) %|% bfgs(max_iter = 50)
  result <- solver(problem, c(0, 0))

  expect_true(is_mle_numerical(result))
  expect_true(abs(result$theta.hat[1] - 3) < 0.5)
  expect_true(abs(result$theta.hat[2] - (-1)) < 0.5)
})
