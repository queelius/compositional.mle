# Tests that algebraic.mle generics dispatch correctly on solver results.
# These verify that Depends: algebraic.mle makes generics available.

test_that("algebraic.mle generics work on gradient_ascent result", {
  problem <- make_normal_problem()
  result <- gradient_ascent(max_iter = 200)(problem, c(0, 1))

  expect_length(params(result), 2)
  expect_equal(params(result)[1], 5, tolerance = 0.5)
  expect_length(se(result), 2)
  expect_true(all(se(result) > 0))
  expect_true(is.numeric(loglik_val(result)))
  expect_equal(nparams(result), 2)
  expect_true(is.numeric(aic(result)))
  expect_equal(aic(result), -2 * loglik_val(result) + 2 * nparams(result))
  expect_true(is.matrix(vcov(result)))
  expect_equal(dim(vcov(result)), c(2, 2))

  ci <- confint(result)
  expect_true(is.matrix(ci))
  expect_equal(nrow(ci), 2)
  expect_equal(ncol(ci), 2)
  # Lower bound should be less than estimate

  expect_true(all(ci[, 1] < params(result)))
  # Upper bound should be greater than estimate
  expect_true(all(ci[, 2] > params(result)))
})

test_that("algebraic.mle generics work on newton_raphson result", {
  problem <- make_normal_problem()
  result <- newton_raphson(max_iter = 50)(problem, c(4, 1.5))

  expect_length(params(result), 2)
  expect_true(is.numeric(loglik_val(result)))
  expect_length(se(result), 2)
  expect_true(is.numeric(aic(result)))
  expect_true(is.matrix(vcov(result)))
})

test_that("algebraic.mle generics work on bfgs result", {
  problem <- make_normal_problem()
  result <- bfgs(max_iter = 100)(problem, c(4, 1.5))

  expect_length(params(result), 2)
  expect_true(is.numeric(loglik_val(result)))
  expect_length(se(result), 2)
  expect_true(is.matrix(vcov(result)))
})

test_that("algebraic.mle generics work on composed chain result", {
  problem <- make_normal_problem()
  strategy <- gradient_ascent(max_iter = 100) %>>% newton_raphson(max_iter = 20)
  result <- strategy(problem, c(0, 1))

  expect_length(params(result), 2)
  expect_equal(params(result)[1], 5, tolerance = 0.5)
  expect_true(is.numeric(loglik_val(result)))
  expect_length(se(result), 2)
  expect_true(is.numeric(aic(result)))
  expect_true(is.matrix(vcov(result)))
})

test_that("algebraic.mle generics work on race result", {
  problem <- make_normal_problem()
  strategy <- gradient_ascent(max_iter = 100) %|% bfgs(max_iter = 100)
  result <- strategy(problem, c(4, 1.5))

  expect_length(params(result), 2)
  expect_true(is.numeric(loglik_val(result)))
  expect_length(se(result), 2)
  expect_true(is.numeric(aic(result)))
  expect_true(is.matrix(vcov(result)))
})

test_that("algebraic.mle generics work on with_restarts result", {
  problem <- make_normal_problem()
  sampler <- uniform_sampler(c(0, 0.5), c(10, 4))
  strategy <- with_restarts(gradient_ascent(max_iter = 100), n = 5,
                            sampler = sampler)
  result <- strategy(problem, c(0, 1))

  expect_length(params(result), 2)
  expect_true(is.numeric(loglik_val(result)))
  expect_length(se(result), 2)
  expect_true(is.numeric(aic(result)))
})
