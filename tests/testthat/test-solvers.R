# Tests for solver factories

test_that("gradient_ascent solves normal MLE", {
  problem <- make_normal_problem()
  solver <- gradient_ascent(max_iter = 200)

  result <- solver(problem, c(0, 1))

  expect_true(is_mle_numerical(result))
  expect_equal(result$theta.hat[1], 5, tolerance = 0.5)
  expect_equal(result$theta.hat[2], 2, tolerance = 0.5)
})

test_that("newton_raphson solves normal MLE", {
  problem <- make_normal_problem()
  solver <- newton_raphson(max_iter = 50)

  result <- solver(problem, c(4, 1.5))

  expect_true(is_mle_numerical(result))
  expect_equal(result$theta.hat[1], 5, tolerance = 0.5)
  expect_equal(result$theta.hat[2], 2, tolerance = 0.5)
})

test_that("bfgs solves normal MLE", {
  problem <- make_normal_problem()
  solver <- bfgs(max_iter = 100)

  # BFGS needs reasonable starting point to avoid overshooting
  result <- solver(problem, c(4, 1.5))

  expect_true(is_mle_numerical(result))
  expect_equal(result$theta.hat[1], 5, tolerance = 0.5)
  expect_equal(result$theta.hat[2], 2, tolerance = 0.5)
})

test_that("nelder_mead solves normal MLE", {
  problem <- make_normal_problem()
  solver <- nelder_mead(max_iter = 500)

  result <- solver(problem, c(0, 1))

  expect_true(is_mle_numerical(result))
  expect_equal(result$theta.hat[1], 5, tolerance = 0.5)
  expect_equal(result$theta.hat[2], 2, tolerance = 0.5)
})

test_that("grid_search finds reasonable starting point", {
  problem <- make_normal_problem()
  solver <- grid_search(lower = c(0, 0.5), upper = c(10, 4), n = 10)

  result <- solver(problem, c(0, 1))

  expect_true(is_mle_numerical(result))
  # Grid search won't be exact but should be in ballpark
  expect_true(abs(result$theta.hat[1] - 5) < 3)
  expect_true(abs(result$theta.hat[2] - 2) < 2)
})

test_that("random_search finds reasonable starting point", {
  problem <- make_normal_problem()
  sampler <- uniform_sampler(c(0, 0.5), c(10, 4))
  solver <- random_search(sampler, n = 100)

  result <- solver(problem, c(0, 1))

  expect_true(is_mle_numerical(result))
})
