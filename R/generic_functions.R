#' Check if solver converged
#'
#' @param x An mle result object
#' @param ... Additional arguments (unused)
#' @return Logical indicating convergence
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
#' @return Logical
#' @export
is_mle_numerical <- function(x) {
  inherits(x, "mle_numerical")
}

#' Get number of iterations
#'
#' @param x An mle result object
#' @param ... Additional arguments (unused)
#' @return Number of iterations
#' @export
num_iterations <- function(x, ...) {
  UseMethod("num_iterations")
}

#' @export
num_iterations.mle_numerical <- function(x, ...) {
  x$iterations %||% x$iter %||% NA_integer_
}

#' @export
num_iterations.default <- function(x, ...) {
  x$iterations %||% x$iter %||% NA_integer_
}

# Null coalescing operator (internal)
# @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
