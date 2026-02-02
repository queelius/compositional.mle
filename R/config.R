#' Create optimization configuration
#'
#' Creates a base configuration object for MLE optimization algorithms.
#' This object stores convergence criteria and debugging options.
#'
#' @param max_iter Maximum iterations (integer, default: 100)
#' @param abs_tol Absolute tolerance (numeric or NULL, default: NULL to use rel_tol)
#' @param rel_tol Relative tolerance (numeric, default: 1e-5)
#' @param trace Store optimization path (logical, default: FALSE)
#' @param debug Print debug information (logical, default: FALSE)
#' @param debug_freq Debug output frequency (integer, default: 1)
#' @return An mle_config object
#' @examples
#' # Basic configuration
#' config <- mle_config(max_iter = 200, rel_tol = 1e-6)
#'
#' # Configuration with tracing
#' config <- mle_config(trace = TRUE, debug = TRUE)
#' @export
mle_config <- function(
  max_iter = 100L,
  abs_tol = NULL,
  rel_tol = 1e-5,
  trace = FALSE,
  debug = FALSE,
  debug_freq = 1L
) {
  max_iter <- as.integer(max_iter)
  debug_freq <- as.integer(debug_freq)

  stopifnot(
    is.integer(max_iter), max_iter > 0,
    is.null(abs_tol) || (is.numeric(abs_tol) && abs_tol > 0),
    is.numeric(rel_tol), rel_tol > 0,
    is.logical(trace),
    is.logical(debug),
    is.integer(debug_freq), debug_freq > 0
  )

  structure(
    list(
      max_iter = max_iter,
      abs_tol = abs_tol,
      rel_tol = rel_tol,
      trace = trace,
      debug = debug,
      debug_freq = debug_freq
    ),
    class = "mle_config"
  )
}

#' Create gradient-based optimization configuration
#'
#' Extends base configuration with gradient-specific parameters like
#' learning rate and distance metric.
#'
#' @inheritParams mle_config
#' @param eta Learning rate / step size (numeric, default: 1.0)
#' @param norm Distance measure function (default: max absolute value)
#' @return An mle_config_gradient object
#' @examples
#' # Basic gradient configuration
#' config <- mle_config_gradient(eta = 0.1, max_iter = 500)
#'
#' # With custom norm (L2 norm)
#' config <- mle_config_gradient(
#'   eta = 0.01,
#'   norm = function(x) sqrt(sum(x^2))
#' )
#' @export
mle_config_gradient <- function(
  eta = 1.0,
  norm = function(x) max(abs(x)),
  max_iter = 100L,
  abs_tol = NULL,
  rel_tol = 1e-5,
  trace = FALSE,
  debug = FALSE,
  debug_freq = 1L
) {
  base_config <- mle_config(max_iter, abs_tol, rel_tol, trace, debug, debug_freq)

  stopifnot(
    is.numeric(eta), eta > 0,
    is.function(norm)
  )

  structure(
    c(base_config,
      list(
        eta = eta,
        norm = norm
      )),
    class = c("mle_config_gradient", "mle_config")
  )
}

#' Create line search configuration
#'
#' Extends gradient configuration with backtracking line search parameters.
#' Line search adaptively finds step sizes that improve the objective.
#'
#' @inheritParams mle_config_gradient
#' @param max_step Maximum step size per iteration (numeric, default: 1.0)
#' @param backtrack_ratio Backtracking multiplier, must be in (0,1) (default: 0.5)
#' @param max_iter_ls Maximum line search iterations (integer, default: 10)
#' @param min_step Minimum step size threshold (numeric, default: 1e-8)
#' @return An mle_config_linesearch object
#' @examples
#' # Conservative line search
#' config <- mle_config_linesearch(
#'   max_step = 0.5,
#'   backtrack_ratio = 0.8
#' )
#'
#' # Aggressive line search
#' config <- mle_config_linesearch(
#'   max_step = 10.0,
#'   backtrack_ratio = 0.3,
#'   max_iter_ls = 20
#' )
#' @export
mle_config_linesearch <- function(
  max_step = 1.0,
  backtrack_ratio = 0.5,
  max_iter_ls = 10L,
  min_step = 1e-8,
  norm = function(x) max(abs(x)),
  max_iter = 100L,
  abs_tol = NULL,
  rel_tol = 1e-5,
  trace = FALSE,
  debug = FALSE,
  debug_freq = 1L
) {
  base_config <- mle_config_gradient(
    eta = max_step, norm = norm,
    max_iter = max_iter, abs_tol = abs_tol, rel_tol = rel_tol,
    trace = trace, debug = debug, debug_freq = debug_freq
  )

  max_iter_ls <- as.integer(max_iter_ls)

  stopifnot(
    is.numeric(backtrack_ratio), backtrack_ratio > 0, backtrack_ratio < 1,
    is.integer(max_iter_ls), max_iter_ls > 0,
    is.numeric(min_step), min_step > 0
  )

  structure(
    c(base_config,
      list(
        backtrack_ratio = backtrack_ratio,
        max_iter_ls = max_iter_ls,
        min_step = min_step
      )),
    class = c("mle_config_linesearch", "mle_config_gradient", "mle_config")
  )
}

#' Create domain constraint specification
#'
#' Specifies domain constraints for optimization. The support function
#' checks if parameters are valid, and the project function maps invalid
#' parameters back to valid ones.
#'
#' @param support Function testing if theta is in support (returns TRUE/FALSE)
#' @param project Function projecting theta onto support
#' @return An mle_constraint object
#' @examples
#' # Positive parameters only
#' constraint <- mle_constraint(
#'   support = function(theta) all(theta > 0),
#'   project = function(theta) pmax(theta, 1e-8)
#' )
#'
#' # Parameters in [0, 1]
#' constraint <- mle_constraint(
#'   support = function(theta) all(theta >= 0 & theta <= 1),
#'   project = function(theta) pmax(0, pmin(1, theta))
#' )
#'
#' # No constraints (default)
#' constraint <- mle_constraint()
#' @export
mle_constraint <- function(
  support = function(theta) TRUE,
  project = function(theta) theta
) {
  stopifnot(
    is.function(support),
    is.function(project)
  )

  structure(
    list(
      support = support,
      project = project
    ),
    class = "mle_constraint"
  )
}

#' Check if object is an mle_config
#'
#' @param x Object to test
#' @return Logical
#' @export
is_mle_config <- function(x) {
  inherits(x, "mle_config")
}

#' Check if object is an mle_constraint
#'
#' @param x Object to test
#' @return Logical
#' @export
is_mle_constraint <- function(x) {
  inherits(x, "mle_constraint")
}
