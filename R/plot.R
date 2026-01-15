#' Plot Optimization Convergence
#'
#' Visualizes the optimization trajectory from an MLE result with tracing enabled.
#' Shows log-likelihood progression, gradient norm decay, and optionally the
#' parameter path (for 2D problems).
#'
#' @param x An mle_numerical result object with trace_data
#' @param which Character vector specifying which plots to show:
#'   "loglike" (log-likelihood), "gradient" (gradient norm), "path" (2D parameter path)
#' @param main Optional title
#' @param ... Additional arguments passed to plot
#' @return Invisibly returns the trace data
#'
#' @details
#' This function requires that the solver was run with tracing enabled via
#' \code{mle_trace()}. Without trace data, the function will warn and return
#' invisibly.
#'
#' The "path" plot is only shown for 2D parameter problems.
#'
#' @examples
#' \donttest{
#' # Enable tracing when solving
#' problem <- mle_problem(
#'   loglike = function(theta) -sum((theta - c(3, 2))^2),
#'   constraint = mle_constraint(support = function(theta) TRUE)
#' )
#' trace_cfg <- mle_trace(values = TRUE, gradients = TRUE, path = TRUE)
#' result <- gradient_ascent(max_iter = 50)(problem, c(0, 0), trace = trace_cfg)
#'
#' # Plot convergence diagnostics
#' plot(result)
#' }
#'
#' @export
plot.mle_numerical <- function(x, which = c("loglike", "gradient"), main = NULL, ...) {
  trace <- x$trace_data

  if (is.null(trace)) {
    warning("No trace data available. Run solver with trace = mle_trace(values = TRUE, ...)")
    return(invisible(NULL))
  }

  which <- match.arg(which, c("loglike", "gradient", "path"), several.ok = TRUE)

  # Determine layout
  n_plots <- sum(c(
    "loglike" %in% which && !is.null(trace$values),
    "gradient" %in% which && !is.null(trace$gradients),
    "path" %in% which && !is.null(trace$path) && ncol(trace$path) == 2
  ))

  if (n_plots == 0) {
    warning("No plottable data in trace (check trace configuration)")
    return(invisible(trace))
  }

  # Set up multi-panel layout
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  if (n_plots > 1) {
    par(mfrow = c(1, n_plots))
  }

  # Plot log-likelihood trajectory
  if ("loglike" %in% which && !is.null(trace$values)) {
    iterations <- seq_along(trace$values)
    plot(iterations, trace$values,
         type = "l", lwd = 2, col = "steelblue",
         xlab = "Iteration", ylab = "Log-likelihood",
         main = if (!is.null(main)) main else "Log-Likelihood Convergence",
         ...)
    points(length(trace$values), trace$values[length(trace$values)],
           pch = 19, col = "red", cex = 1.5)
  }

  # Plot gradient norm decay
  if ("gradient" %in% which && !is.null(trace$gradients)) {
    iterations <- seq_along(trace$gradients)
    # Use log scale if values span multiple orders of magnitude
    use_log <- max(trace$gradients) / min(trace$gradients[trace$gradients > 0]) > 100

    if (use_log) {
      plot(iterations, trace$gradients,
           type = "l", lwd = 2, col = "darkgreen",
           xlab = "Iteration", ylab = "Gradient Norm (log scale)",
           main = "Gradient Norm Decay", log = "y", ...)
    } else {
      plot(iterations, trace$gradients,
           type = "l", lwd = 2, col = "darkgreen",
           xlab = "Iteration", ylab = "Gradient Norm",
           main = "Gradient Norm Decay", ...)
    }
    abline(h = 0, lty = 2, col = "gray")
  }

  # Plot 2D parameter path
  if ("path" %in% which && !is.null(trace$path) && ncol(trace$path) == 2) {
    plot(trace$path[, 1], trace$path[, 2],
         type = "l", lwd = 1.5, col = "purple",
         xlab = expression(theta[1]), ylab = expression(theta[2]),
         main = "Parameter Path", ...)
    # Mark start and end
    points(trace$path[1, 1], trace$path[1, 2],
           pch = 1, col = "blue", cex = 2, lwd = 2)
    points(trace$path[nrow(trace$path), 1], trace$path[nrow(trace$path), 2],
           pch = 19, col = "red", cex = 1.5)
    legend("topright", c("Start", "MLE"), pch = c(1, 19), col = c("blue", "red"))
  }

  invisible(trace)
}


#' Extract Optimization Path as Data Frame
#'
#' Converts the trace data from an MLE result into a tidy data frame for
#' custom analysis and plotting (e.g., with ggplot2).
#'
#' @param x An mle_numerical result object with trace_data, or an mle_trace_data object
#' @param ... Additional arguments (unused)
#' @return A data frame with columns:
#'   \itemize{
#'     \item \code{iteration}: Iteration number
#'     \item \code{loglike}: Log-likelihood value (if traced)
#'     \item \code{grad_norm}: Gradient norm (if traced)
#'     \item \code{time}: Elapsed time in seconds (if traced)
#'     \item \code{theta_1}, \code{theta_2}, ...: Parameter values (if path traced)
#'   }
#'
#' @examples
#' \donttest{
#' # Get optimization path as data frame
#' problem <- mle_problem(
#'   loglike = function(theta) -sum((theta - c(3, 2))^2),
#'   constraint = mle_constraint(support = function(theta) TRUE)
#' )
#' trace_cfg <- mle_trace(values = TRUE, path = TRUE)
#' result <- gradient_ascent(max_iter = 30)(problem, c(0, 0), trace = trace_cfg)
#'
#' path_df <- optimization_path(result)
#' head(path_df)
#' }
#'
#' @export
optimization_path <- function(x, ...) {
  UseMethod("optimization_path")
}

#' @export
optimization_path.mle_numerical <- function(x, ...) {
  if (is.null(x$trace_data)) {
    warning("No trace data available. Run solver with trace = mle_trace(path = TRUE, ...)")
    return(NULL)
  }
  optimization_path.mle_trace_data(x$trace_data, ...)
}

#' @export
optimization_path.mle_trace_data <- function(x, ...) {
  # Determine number of recorded iterations
  n_iter <- NULL
  if (!is.null(x$values)) n_iter <- length(x$values)
  if (!is.null(x$gradients)) n_iter <- length(x$gradients)
  if (!is.null(x$path)) n_iter <- nrow(x$path)
  if (!is.null(x$times)) n_iter <- length(x$times)

  if (is.null(n_iter) || n_iter == 0) {
    warning("No trace data to extract")
    return(NULL)
  }

  # Build data frame
  df <- data.frame(iteration = seq_len(n_iter))

  if (!is.null(x$values)) {
    df$loglike <- x$values
  }

  if (!is.null(x$gradients)) {
    df$grad_norm <- x$gradients
  }

  if (!is.null(x$times)) {
    df$time <- x$times
  }

  if (!is.null(x$path)) {
    path_df <- as.data.frame(x$path)
    n_params <- ncol(path_df)
    names(path_df) <- paste0("theta_", seq_len(n_params))
    df <- cbind(df, path_df)
  }

  df
}


#' Plot Trace Data Directly
#'
#' @param x An mle_trace_data object
#' @param ... Arguments passed to plotting functions
#' @export
plot.mle_trace_data <- function(x, ...) {
  # Create a wrapper object to use plot.mle_numerical
  wrapper <- list(trace_data = x)
  class(wrapper) <- "mle_numerical"
  plot.mle_numerical(wrapper, ...)
}
