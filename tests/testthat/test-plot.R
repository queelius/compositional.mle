test_that("plot.mle_numerical works with trace data", {
  problem <- mle_problem(
    loglike = function(theta) -sum((theta - c(3, 2))^2),
    score = function(theta) -2 * (theta - c(3, 2))
  )

  # Solve with tracing
  trace_cfg <- mle_trace(values = TRUE, gradients = TRUE, path = TRUE)
  result <- gradient_ascent(max_iter = 20)(problem, c(0, 0), trace = trace_cfg)

  # Should not error when plotting
  expect_silent({
    pdf(NULL)  # Null device to suppress actual plotting
    on.exit(dev.off())
    plot(result)
  })

  # Check trace data is returned invisibly
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  trace_out <- plot(result)
  expect_s3_class(trace_out, "mle_trace_data")
})

test_that("plot.mle_numerical warns when no trace data", {
  problem <- mle_problem(
    loglike = function(theta) -sum((theta - c(3, 2))^2)
  )

  # Solve WITHOUT tracing
  result <- gradient_ascent(max_iter = 10)(problem, c(0, 0))

  # Should warn
  expect_warning(plot(result), "No trace data")
})

test_that("optimization_path extracts trace as data frame", {
  problem <- mle_problem(
    loglike = function(theta) -sum((theta - c(3, 2))^2),
    score = function(theta) -2 * (theta - c(3, 2))
  )

  # Solve with full tracing
  trace_cfg <- mle_trace(values = TRUE, gradients = TRUE, path = TRUE, timing = TRUE)
  result <- gradient_ascent(max_iter = 15)(problem, c(0, 0), trace = trace_cfg)

  # Extract path
  path_df <- optimization_path(result)

  # Should be a data frame
  expect_s3_class(path_df, "data.frame")

  # Should have expected columns
  expect_true("iteration" %in% names(path_df))
  expect_true("loglike" %in% names(path_df))
  expect_true("grad_norm" %in% names(path_df))
  expect_true("time" %in% names(path_df))
  expect_true("theta_1" %in% names(path_df))
  expect_true("theta_2" %in% names(path_df))

  # Should have reasonable values
  expect_true(nrow(path_df) > 0)
  expect_true(all(path_df$iteration == seq_len(nrow(path_df))))

  # Log-likelihood should generally increase
  expect_true(path_df$loglike[nrow(path_df)] > path_df$loglike[1])
})

test_that("optimization_path warns when no trace data", {
  problem <- mle_problem(
    loglike = function(theta) -sum((theta - c(3, 2))^2)
  )

  # Solve WITHOUT tracing
  result <- gradient_ascent(max_iter = 10)(problem, c(0, 0))

  # Should warn and return NULL
  expect_warning(path <- optimization_path(result), "No trace data")
  expect_null(path)
})

test_that("plot.mle_trace_data works directly", {
  problem <- mle_problem(
    loglike = function(theta) -sum((theta - c(3, 2))^2),
    score = function(theta) -2 * (theta - c(3, 2))
  )

  # Solve with tracing
  trace_cfg <- mle_trace(values = TRUE, path = TRUE)
  result <- gradient_ascent(max_iter = 20)(problem, c(0, 0), trace = trace_cfg)

  # Should be able to plot trace_data directly
  expect_silent({
    pdf(NULL)
    on.exit(dev.off())
    plot(result$trace_data)
  })
})

test_that("optimization_path works on trace_data directly", {
  problem <- mle_problem(
    loglike = function(theta) -sum((theta - c(3, 2))^2)
  )

  # Solve with tracing
  trace_cfg <- mle_trace(values = TRUE, path = TRUE)
  result <- gradient_ascent(max_iter = 15)(problem, c(0, 0), trace = trace_cfg)

  # Should work on trace_data directly
  path_df <- optimization_path(result$trace_data)

  expect_s3_class(path_df, "data.frame")
  expect_true(nrow(path_df) > 0)
})
