test_that("mle_problem.likelihood_model creates valid problem", {
  skip_if_not_installed("likelihood.model")
  model <- likelihood.model::exponential_lifetime("t")
  set.seed(42)
  data <- data.frame(t = rexp(50, rate = 2))
  problem <- mle_problem(model, data)
  expect_s3_class(problem, "mle_problem")
  expect_equal(problem$n_obs, 50)
  result <- bfgs()(problem, c(1))
  expect_true(is_solver_result(result))
  expect_true(abs(params(result) - 2) < 1)
})

test_that("mle_problem.default still works with named loglike arg", {
  problem <- mle_problem(
    loglike = function(theta) -sum((theta - 3)^2),
    theta_names = "mu"
  )
  expect_s3_class(problem, "mle_problem")
  expect_equal(problem$theta_names, "mu")
})
