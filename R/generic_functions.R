#' Check if solver converged
#'
#' @param x An mle result object
#' @param ... Additional arguments (unused)
#' @return Logical indicating convergence
#' @examples
#' \donttest{
#' problem <- mle_problem(
#'   loglike = function(theta) -sum((theta - c(1, 2))^2)
#' )
#' result <- gradient_ascent(max_iter = 50)(problem, c(0, 0))
#' is_converged(result)
#' }
#' @export
is_converged <- function(x, ...) {
  UseMethod("is_converged")
}

#' @export
is_converged.mle_numerical <- function(x, ...) {
  isTRUE(x$converged)
}

#' @export
is_converged.default <- function(x, ...) {
  if (!is.null(x$converged)) {
    isTRUE(x$converged)
  } else if (!is.null(x$convergence)) {
    x$convergence == 0
  } else {
    NA
  }
}

#' Check if object is an mle_numerical
#'
#' @param x Object to test
#' @return Logical indicating whether \code{x} inherits from \code{mle_numerical}.
#' @examples
#' \donttest{
#' problem <- mle_problem(
#'   loglike = function(theta) -sum((theta - c(1, 2))^2)
#' )
#' result <- gradient_ascent(max_iter = 20)(problem, c(0, 0))
#' is_mle_numerical(result)  # TRUE
#' is_mle_numerical(list())  # FALSE
#' }
#' @export
is_mle_numerical <- function(x) {
  inherits(x, "mle_numerical")
}

#' Get number of iterations
#'
#' @param x An mle result object
#' @param ... Additional arguments (unused)
#' @return Number of iterations, or \code{NA_integer_} if not available.
#' @examples
#' \donttest{
#' problem <- mle_problem(
#'   loglike = function(theta) -sum((theta - c(1, 2))^2)
#' )
#' result <- gradient_ascent(max_iter = 50)(problem, c(0, 0))
#' num_iterations(result)
#' }
#' @export
num_iterations <- function(x, ...) {
  UseMethod("num_iterations")
}

#' @export
num_iterations.default <- function(x, ...) {
  x$iterations %||% x$iter %||% NA_integer_
}

# Null coalescing operator (internal)
# @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
