test_that("mle_constraint creates valid constraint objects", {
  # Default (unconstrained)
  constraint <- mle_constraint()
  expect_s3_class(constraint, "mle_constraint")
  expect_true(is.function(constraint$support))
  expect_true(is.function(constraint$project))

  # Support should accept anything by default
  expect_true(constraint$support(c(-10, 20, 30)))
  expect_true(constraint$support(c(0, 0, 0)))

  # Project should be identity by default
  theta <- c(1, -2, 3)
  expect_equal(constraint$project(theta), theta)
})

test_that("mle_constraint with custom functions works", {
  # Positive constraint
  constraint <- mle_constraint(
    support = function(theta) all(theta > 0),
    project = function(theta) pmax(theta, 1e-8)
  )

  # Test support
  expect_true(constraint$support(c(1, 2, 3)))
  expect_false(constraint$support(c(1, -2, 3)))
  expect_false(constraint$support(c(0, 1, 2)))

  # Test projection
  expect_equal(constraint$project(c(1, -2, 3)), c(1, 1e-8, 3))
  expect_equal(constraint$project(c(-1, -2, -3)), c(1e-8, 1e-8, 1e-8))
})

test_that("mle_constraint validates inputs", {
  expect_error(mle_constraint(support = "not a function"))
  expect_error(mle_constraint(project = 42))
  expect_error(mle_constraint(support = NULL))
})

test_that("mle_constraint with box constraints works", {
  # Box constraints: 0 <= theta <= 1
  constraint <- mle_constraint(
    support = function(theta) all(theta >= 0 & theta <= 1),
    project = function(theta) pmax(0, pmin(1, theta))
  )

  # Test support
  expect_true(constraint$support(c(0.5, 0.5)))
  expect_true(constraint$support(c(0, 1)))
  expect_false(constraint$support(c(-0.1, 0.5)))
  expect_false(constraint$support(c(0.5, 1.1)))

  # Test projection
  expect_equal(constraint$project(c(-0.5, 0.5, 1.5)), c(0, 0.5, 1))
  expect_equal(constraint$project(c(0.3, 2.0)), c(0.3, 1.0))
})

test_that("is_mle_constraint works correctly", {
  expect_true(is_mle_constraint(mle_constraint()))
  expect_false(is_mle_constraint(list(support = function(x) TRUE)))
  expect_false(is_mle_constraint("not a constraint"))
  expect_false(is_mle_constraint(NULL))
})

test_that("Constraint objects are reusable", {
  # Create a constraint and use it multiple times
  constraint <- mle_constraint(
    support = function(theta) all(theta > 0),
    project = function(theta) pmax(theta, 0.01)
  )

  # Should work for different parameter vectors
  expect_true(constraint$support(c(1, 2)))
  expect_false(constraint$support(c(-1, 2)))
  expect_equal(constraint$project(c(-1, 2)), c(0.01, 2))
  expect_equal(constraint$project(c(0, 3)), c(0.01, 3))
})
