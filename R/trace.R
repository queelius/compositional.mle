#' Create a Trace Configuration
#'
#' Specifies what information to track during optimization.
#'
#' @param values Track log-likelihood values at each iteration
#' @param path Track parameter values at each iteration
#' @param gradients Track gradient norms at each iteration
#' @param timing Track wall-clock time
#' @param every Record every nth iteration (1 = all iterations)
#' @return An mle_trace configuration object
#'
#' @examples
#' # Track everything
#' trace <- mle_trace(values = TRUE, path = TRUE, gradients = TRUE)
#'
#' # Minimal tracing (just convergence path)
#' trace <- mle_trace(values = TRUE)
#'
#' # Sample every 10th iteration for long runs
#' trace <- mle_trace(values = TRUE, path = TRUE, every = 10)
#'
#' @export
mle_trace <- function(
  values = FALSE,
  path = FALSE,
  gradients = FALSE,
  timing = FALSE,
  every = 1L
) {
  structure(
    list(
      values = values,
      path = path,
      gradients = gradients,
      timing = timing,
      every = as.integer(every)
    ),
    class = "mle_trace"
  )
}

#' Check if tracing is enabled
#'
#' @param trace An mle_trace object
#' @return Logical indicating if any tracing is enabled
#' @export
is_tracing <- function(trace) {
  trace$values || trace$path || trace$gradients || trace$timing
}

#' Create a trace recorder
#'
#' Internal function to create a mutable trace recorder.
#'
#' @param trace mle_trace configuration
#' @param n_params Number of parameters (for pre-allocation)
#' @return A trace recorder environment
#' @keywords internal
new_trace_recorder <- function(trace, n_params) {
  if (!is_tracing(trace)) {
    return(NULL)
  }

  env <- new.env(parent = emptyenv())
  env$config <- trace
  env$iteration <- 0L
  env$start_time <- Sys.time()

  if (trace$values) env$values <- numeric(0)
  if (trace$path) env$path <- list()
  if (trace$gradients) env$gradients <- numeric(0)
  if (trace$timing) env$times <- numeric(0)

  env
}

#' Record an iteration to trace
#'
#' @param recorder Trace recorder from new_trace_recorder
#' @param theta Current parameters
#' @param value Current log-likelihood (or NULL)
#' @param gradient Current gradient (or NULL)
#' @keywords internal
record_iteration <- function(recorder, theta, value = NULL, gradient = NULL) {
  if (is.null(recorder)) return(invisible(NULL))

  recorder$iteration <- recorder$iteration + 1L

  # Only record on specified intervals
 if (recorder$iteration %% recorder$config$every != 0) {
    return(invisible(NULL))
  }

  if (recorder$config$values && !is.null(value)) {
    recorder$values <- c(recorder$values, value)
  }

  if (recorder$config$path) {
    recorder$path[[length(recorder$path) + 1]] <- theta
  }

  if (recorder$config$gradients && !is.null(gradient)) {
    grad_norm <- sqrt(sum(gradient^2))
    recorder$gradients <- c(recorder$gradients, grad_norm)
  }

  if (recorder$config$timing) {
    elapsed <- as.numeric(difftime(Sys.time(), recorder$start_time, units = "secs"))
    recorder$times <- c(recorder$times, elapsed)
  }

  invisible(NULL)
}

#' Finalize trace recorder into trace data
#'
#' @param recorder Trace recorder
#' @return List of trace data or NULL
#' @keywords internal
finalize_trace <- function(recorder) {
  if (is.null(recorder)) return(NULL)

  result <- list()

  if (recorder$config$values && length(recorder$values) > 0) {
    result$values <- recorder$values
  }

  if (recorder$config$path && length(recorder$path) > 0) {
    result$path <- do.call(rbind, recorder$path)
  }

  if (recorder$config$gradients && length(recorder$gradients) > 0) {
    result$gradients <- recorder$gradients
  }

  if (recorder$config$timing && length(recorder$times) > 0) {
    result$times <- recorder$times
  }

  result$total_iterations <- recorder$iteration
  result$total_time <- as.numeric(difftime(Sys.time(), recorder$start_time, units = "secs"))

  class(result) <- "mle_trace_data"
  result
}

#' @export
print.mle_trace <- function(x, ...) {
  cat("MLE Trace Configuration\n")
  cat("  Track values:", x$values, "\n")
  cat("  Track path:", x$path, "\n")
  cat("  Track gradients:", x$gradients, "\n")
  cat("  Track timing:", x$timing, "\n")
  if (x$every > 1) cat("  Record every:", x$every, "iterations\n")
  invisible(x)
}

#' @export
print.mle_trace_data <- function(x, ...) {
  cat("MLE Trace Data\n")
  cat("  Total iterations:", x$total_iterations, "\n")
  cat("  Total time:", round(x$total_time, 3), "seconds\n")
  if (!is.null(x$values)) {
    cat("  Log-likelihood: ", round(x$values[1], 4), " -> ", round(tail(x$values, 1), 4), "\n", sep = "")
  }
  if (!is.null(x$path)) {
    cat("  Path recorded:", nrow(x$path), "points\n")
  }
  if (!is.null(x$gradients)) {
    cat("  Gradient norm: ", round(x$gradients[1], 4), " -> ", round(tail(x$gradients, 1), 4), "\n", sep = "")
  }
  if (!is.null(x$stages)) {
    cat("  Stages:", length(x$stages), "(composed trace)\n")
  }
  invisible(x)
}

#' Merge trace data from multiple results
#'
#' Concatenates trace data from a sequence of results (e.g., from composed solvers).
#' The merged trace preserves stage boundaries for later analysis.
#'
#' @param results List of mle_numerical results with trace_data
#' @return A merged mle_trace_data object with stage information
#' @keywords internal
merge_traces <- function(results) {
  # Filter to results with trace data
  traces <- lapply(results, function(r) r$trace_data)
  traces <- Filter(Negate(is.null), traces)

  if (length(traces) == 0) return(NULL)
  if (length(traces) == 1) return(traces[[1]])

  # Merge values
  all_values <- unlist(lapply(traces, function(t) t$values))

  # Merge paths
  all_paths <- lapply(traces, function(t) t$path)
  all_paths <- Filter(Negate(is.null), all_paths)
  merged_path <- if (length(all_paths) > 0) {
    do.call(rbind, all_paths)
  } else NULL

  # Merge gradients
  all_gradients <- unlist(lapply(traces, function(t) t$gradients))

  # Merge times (cumulative)
  all_times <- NULL
  if (!is.null(traces[[1]]$times)) {
    cumulative_offset <- 0
    for (t in traces) {
      if (!is.null(t$times)) {
        all_times <- c(all_times, t$times + cumulative_offset)
        cumulative_offset <- cumulative_offset + t$total_time
      }
    }
  }

  # Stage boundaries (cumulative iteration counts)
  stage_ends <- cumsum(sapply(traces, function(t) {
    if (!is.null(t$values)) length(t$values)
    else if (!is.null(t$path)) nrow(t$path)
    else t$total_iterations
  }))

  # Build merged result
  merged <- list()
  if (length(all_values) > 0) merged$values <- all_values
  if (!is.null(merged_path)) merged$path <- merged_path
  if (length(all_gradients) > 0) merged$gradients <- all_gradients
  if (!is.null(all_times)) merged$times <- all_times

  merged$total_iterations <- sum(sapply(traces, function(t) t$total_iterations))
  merged$total_time <- sum(sapply(traces, function(t) t$total_time))
  merged$stages <- stage_ends

  class(merged) <- "mle_trace_data"
  merged
}
