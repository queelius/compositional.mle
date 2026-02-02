# Tests for generic functions

test_that("mle_numerical objects from algebraic.mle work correctly", {
  # Create a simple mle_numerical object using optim-style output
  sol <- list(
    par = c(0, 1),
    value = -10,
    convergence = 0L,
    hessian = -matrix(c(1, 0, 0, 1), nrow = 2)
  )

  result <- algebraic.mle::mle_numerical(sol = sol)

  expect_true(inherits(result, "mle_numerical"))
  expect_true(inherits(result, "mle"))
  expect_true(result$converged)  # convergence == 0 means converged
  expect_equal(result$theta.hat, c(0, 1))
  expect_equal(result$loglike, -10)
})

test_that("is_converged works correctly", {
  # Converged result (convergence = 0)
  converged_result <- create_test_mle_numerical(
    par = c(0, 1),
    value = -10,
    convergence = 0L,
    iterations = 50L
  )

  # Not converged result (convergence = 1)
  not_converged_result <- create_test_mle_numerical(
    par = c(0, 1),
    value = -10,
    convergence = 1L,
    iterations = 100L
  )

  expect_true(is_converged(converged_result))
  expect_false(is_converged(not_converged_result))
})

test_that("is_mle_numerical works correctly", {
  result <- create_test_mle_numerical(
    par = c(0, 1),
    value = -10,
    convergence = 0L,
    iterations = 50L
  )

  expect_true(is_mle_numerical(result))
  expect_false(is_mle_numerical(list(a = 1)))
  expect_false(is_mle_numerical("string"))
  expect_false(is_mle_numerical(123))
  expect_false(is_mle_numerical(NULL))
})

test_that("num_iterations works correctly", {
  result <- create_test_mle_numerical(
    par = c(0, 1),
    value = -10,
    convergence = 0L,
    iterations = 75L
  )

  expect_equal(num_iterations(result), 75L)
})

test_that("num_iterations handles both field names", {
  # Test with 'iterations' field
  result1 <- create_test_mle_numerical(
    par = c(0, 1),
    value = -10,
    convergence = 0L,
    iterations = 42L
  )
  expect_equal(num_iterations(result1), 42L)

  # Test with legacy 'iter' field
  result2 <- algebraic.mle::mle_numerical(
    sol = list(par = c(0, 1), value = -10, convergence = 0L)
  )
  result2$iter <- 33L
  expect_equal(num_iterations(result2), 33L)
})

test_that("generic functions work with solver results", {
  # Test with actual solver output
  problem <- mle_problem(
    loglike = function(theta) {
      if (theta[2] <= 0) return(-Inf)
      -(theta[1] - 5)^2 - (theta[2] - 2)^2
    },
    score = function(theta) {
      c(-2 * (theta[1] - 5), -2 * (theta[2] - 2))
    },
    constraint = mle_constraint(
      support = function(theta) theta[2] > 0,
      project = function(theta) c(theta[1], max(theta[2], 1e-6))
    )
  )

  solver <- gradient_ascent(max_iter = 200)
  result <- solver(problem, c(0, 1))

  expect_true(is_mle_numerical(result))
  expect_true(is.logical(is_converged(result)))
  expect_true(is.numeric(num_iterations(result)))
  expect_true(num_iterations(result) > 0)
})

test_that("is_mle_problem works correctly", {
  problem <- mle_problem(
    loglike = function(theta) -sum(theta^2)
  )

  expect_true(is_mle_problem(problem))
  expect_false(is_mle_problem(list(loglike = function(x) x)))
  expect_false(is_mle_problem(NULL))
})

test_that("is_mle_constraint works correctly", {
  constraint <- mle_constraint(
    support = function(theta) all(theta > 0),
    project = function(theta) pmax(theta, 0)
  )

  expect_true(is_mle_constraint(constraint))
  expect_false(is_mle_constraint(list(support = function(x) TRUE)))
  expect_false(is_mle_constraint(NULL))
})
