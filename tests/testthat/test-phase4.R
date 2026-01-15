# Tests for Phase 4 features

# Standard test problem
make_test_problem <- function(cache = FALSE) {
  x <- rnorm(50, mean = 5, sd = 2)
  mle_problem(
    loglike = function(theta) {
      if (theta[2] <= 0) return(-Inf)
      sum(dnorm(x, theta[1], theta[2], log = TRUE))
    },
    constraint = mle_constraint(
      support = function(theta) theta[2] > 0,
      project = function(theta) c(theta[1], max(theta[2], 0.01))
    ),
    cache_derivatives = cache
  )
}

# ============================================================================
# Phase 4.1: Derivative Caching
# ============================================================================

test_that("mle_problem accepts cache_derivatives parameter", {
  problem <- make_test_problem(cache = TRUE)
  expect_true(problem$cache_derivatives)

  problem2 <- make_test_problem(cache = FALSE)
  expect_false(problem2$cache_derivatives)
})

test_that("cached score returns same result on repeated calls", {
  problem <- make_test_problem(cache = TRUE)
  score_fn <- get_score(problem)

  theta <- c(4, 2)
  result1 <- score_fn(theta)
  result2 <- score_fn(theta)

  expect_equal(result1, result2)
})

test_that("cached score updates when theta changes", {
  problem <- make_test_problem(cache = TRUE)
  score_fn <- get_score(problem)

  theta1 <- c(4, 2)
  theta2 <- c(5, 2)

  result1 <- score_fn(theta1)
  result2 <- score_fn(theta2)

  expect_false(isTRUE(all.equal(result1, result2)))
})

test_that("clear_cache removes cached values", {
  problem <- make_test_problem(cache = TRUE)
  score_fn <- get_score(problem)

  # Populate cache
  score_fn(c(4, 2))
  expect_true(length(ls(problem$.cache)) > 0)

  # Clear cache
  clear_cache(problem)
  expect_equal(length(ls(problem$.cache)), 0)
})

test_that("cached fisher returns same result on repeated calls", {
  problem <- make_test_problem(cache = TRUE)
  fisher_fn <- get_fisher(problem)

  theta <- c(4, 2)
  result1 <- fisher_fn(theta)
  result2 <- fisher_fn(theta)

  expect_equal(result1, result2)
})

test_that("print.mle_problem shows caching status", {
  problem <- make_test_problem(cache = TRUE)
  output <- capture.output(print(problem))

  expect_true(any(grepl("cached", output)))
})

test_that("solver works with cached derivatives", {
  problem <- make_test_problem(cache = TRUE)
  result <- gradient_ascent(max_iter = 50)(problem, c(3, 1))

  expect_true(result$converged || result$iterations > 0)
  expect_true(is.finite(result$loglike))
})

# ============================================================================
# Phase 4.2: Verbose Output (basic tests - can't test actual output easily)
# ============================================================================

test_that("gradient_ascent accepts verbose parameter", {
  solver <- gradient_ascent(verbose = FALSE)
  expect_true(is.function(solver))

  # Should not error
  problem <- make_test_problem()
  result <- solver(problem, c(3, 1))
  expect_true(is.finite(result$loglike))
})

test_that("newton_raphson accepts verbose parameter", {
  solver <- newton_raphson(verbose = FALSE)
  expect_true(is.function(solver))
})

test_that("sim_anneal accepts verbose parameter", {
  solver <- sim_anneal(max_iter = 50, verbose = FALSE)
  expect_true(is.function(solver))
})

test_that("coordinate_ascent accepts verbose parameter", {
  solver <- coordinate_ascent(max_cycles = 10, verbose = FALSE)
  expect_true(is.function(solver))
})

# ============================================================================
# Phase 4.3: Trace Aggregation
# ============================================================================

test_that("%>>% merges trace data from both solvers", {
  problem <- make_test_problem()
  trace_cfg <- mle_trace(values = TRUE, path = TRUE)

  strategy <- gradient_ascent(max_iter = 20) %>>% gradient_ascent(max_iter = 10)
  result <- strategy(problem, c(3, 1), trace = trace_cfg)

  expect_s3_class(result$trace_data, "mle_trace_data")
  expect_true(!is.null(result$trace_data$values))

  # Should have more iterations than single solver
  expect_true(length(result$trace_data$values) > 10)
})

test_that("%>>% trace has stage boundaries", {
  problem <- make_test_problem()
  trace_cfg <- mle_trace(values = TRUE)

  strategy <- gradient_ascent(max_iter = 15) %>>% gradient_ascent(max_iter = 10)
  result <- strategy(problem, c(3, 1), trace = trace_cfg)

  # Should have stages field marking boundaries
  expect_true(!is.null(result$trace_data$stages))
  expect_true(length(result$trace_data$stages) >= 2)
})

test_that("compose() merges trace data", {
  problem <- make_test_problem()
  trace_cfg <- mle_trace(values = TRUE)

  strategy <- compose(
    gradient_ascent(max_iter = 10),
    gradient_ascent(max_iter = 10)
  )
  result <- strategy(problem, c(3, 1), trace = trace_cfg)

  expect_s3_class(result$trace_data, "mle_trace_data")
  expect_true(!is.null(result$trace_data$stages))
})

# ============================================================================
# Phase 4.4: race() Function
# ============================================================================

test_that("race() runs multiple solvers and picks best", {
  problem <- make_test_problem()

  strategy <- race(gradient_ascent(max_iter = 30), bfgs())
  result <- strategy(problem, c(3, 1))

  expect_true(result$strategy == "race")
  expect_true(!is.null(result$alternatives))
  expect_true(length(result$alternatives) == 2)
  expect_true(!is.null(result$winner_index))
})

test_that("race() handles solver failures gracefully", {
  problem <- make_test_problem()

  # Create a solver that always fails
  failing_solver <- function(...) {
    function(problem, theta0, trace = mle_trace()) {
      stop("Intentional failure")
    }
  }

  strategy <- race(failing_solver(), gradient_ascent(max_iter = 30))
  result <- strategy(problem, c(3, 1))

  expect_true(is.finite(result$loglike))
  expect_equal(result$winner_index, 2)
})

test_that("race() errors when all solvers fail", {
  problem <- make_test_problem()

  failing_solver <- function(...) {
    function(problem, theta0, trace = mle_trace()) {
      stop("Intentional failure")
    }
  }

  strategy <- race(failing_solver(), failing_solver())
  expect_error(strategy(problem, c(3, 1)), "All solvers in race failed")
})

test_that("race() works with three or more solvers", {
  problem <- make_test_problem()

  strategy <- race(
    gradient_ascent(max_iter = 20),
    bfgs(),
    nelder_mead(max_iter = 50)
  )
  result <- strategy(problem, c(3, 1))

  expect_true(length(result$alternatives) == 3)
  expect_true(result$winner_index %in% 1:3)
})

# ============================================================================
# Phase 4.5: chain() Function with Early Stopping
# ============================================================================

test_that("chain() runs solvers sequentially", {
  problem <- make_test_problem()

  strategy <- chain(
    gradient_ascent(max_iter = 20),
    gradient_ascent(max_iter = 10)
  )
  result <- strategy(problem, c(3, 1))

  expect_equal(result$strategy, "chain")
  expect_true(!is.null(result$chain))
  expect_equal(length(result$chain), 2)
})

test_that("chain() stops early when condition is met", {
  problem <- make_test_problem()

  # Early stop function that always returns TRUE
  always_stop <- function(r) TRUE

  strategy <- chain(
    gradient_ascent(max_iter = 20),
    gradient_ascent(max_iter = 20),
    newton_raphson(max_iter = 20),
    early_stop = always_stop
  )
  result <- strategy(problem, c(3, 1))

  # Should have stopped after first solver
  expect_true(result$stopped_early)
  expect_equal(length(result$chain), 1)
})

test_that("chain() continues when early_stop returns FALSE", {
  problem <- make_test_problem()

  # Early stop function that never returns TRUE
  never_stop <- function(r) FALSE

  strategy <- chain(
    gradient_ascent(max_iter = 10),
    gradient_ascent(max_iter = 10),
    early_stop = never_stop
  )
  result <- strategy(problem, c(3, 1))

  expect_false(result$stopped_early)
  expect_equal(length(result$chain), 2)
})

test_that("chain() with convergence-based early stop", {
  problem <- make_test_problem()

  # Stop when converged
  stop_when_converged <- function(r) isTRUE(r$converged)

  strategy <- chain(
    gradient_ascent(max_iter = 100),  # Should converge
    newton_raphson(max_iter = 20),    # Would run if not converged
    early_stop = stop_when_converged
  )
  result <- strategy(problem, c(3, 1))

  # If first solver converged, should have stopped early
  if (result$stopped_early) {
    expect_equal(length(result$chain), 1)
  } else {
    expect_equal(length(result$chain), 2)
  }
})

test_that("chain() merges trace data", {
  problem <- make_test_problem()
  trace_cfg <- mle_trace(values = TRUE)

  strategy <- chain(
    gradient_ascent(max_iter = 15),
    gradient_ascent(max_iter = 10)
  )
  result <- strategy(problem, c(3, 1), trace = trace_cfg)

  expect_s3_class(result$trace_data, "mle_trace_data")
  expect_true(!is.null(result$trace_data$stages))
})

test_that("chain() with single solver returns that solver", {
  problem <- make_test_problem()

  strategy <- chain(gradient_ascent(max_iter = 30))
  result <- strategy(problem, c(3, 1))

  expect_true(is.finite(result$loglike))
})
