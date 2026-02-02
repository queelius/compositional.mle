#' Create an MLE Problem Specification
#'
#' Encapsulates a maximum likelihood estimation problem, separating the
#' statistical specification from the optimization strategy.
#'
#' @param loglike Log-likelihood function taking parameter vector theta
#' @param score Score function (gradient of log-likelihood). If NULL,
#'   computed numerically via numDeriv::grad when needed.
#' @param fisher Fisher information matrix function. If NULL, computed
#'   numerically via numDeriv::hessian when needed.
#' @param constraint Domain constraints as mle_constraint object
#' @param theta_names Character vector of parameter names for nice output
#' @param n_obs Number of observations (for AIC/BIC computation)
#' @param cache_derivatives Logical; if TRUE and score/fisher are computed
#'   numerically, cache the most recent result to avoid redundant computation.
#'   This is particularly useful during line search where the same point may
#'   be evaluated multiple times. Default is FALSE.
#' @return An mle_problem object
#'
#' @details
#' The problem object provides lazy evaluation of derivatives. If you don't
#' provide analytic score or fisher functions, they will be computed
#' numerically when requested.
#'
#' When \code{cache_derivatives = TRUE}, numerical derivatives are cached
#' using a single-value cache (stores the most recent theta and result).
#' This is efficient for optimization where consecutive calls often evaluate
#' at the same point (e.g., during line search or convergence checking).
#' Use \code{\link{clear_cache}} to manually clear the cache if needed.
#'
#' @examples
#' # With analytic derivatives
#' problem <- mle_problem(
#'   loglike = function(theta) sum(dnorm(data, theta[1], theta[2], log = TRUE)),
#'   score = function(theta) {
#'     c(sum(data - theta[1]) / theta[2]^2,
#'       -length(data)/theta[2] + sum((data - theta[1])^2) / theta[2]^3)
#'   },
#'   constraint = mle_constraint(
#'     support = function(theta) theta[2] > 0,
#'     project = function(theta) c(theta[1], max(theta[2], 1e-8))
#'   ),
#'   theta_names = c("mu", "sigma")
#' )
#'
#' # Without analytic derivatives (computed numerically)
#' problem <- mle_problem(
#'   loglike = function(theta) sum(dnorm(data, theta[1], theta[2], log = TRUE)),
#'   constraint = mle_constraint(
#'     support = function(theta) theta[2] > 0
#'   )
#' )
#'
#' @export
mle_problem <- function(
  loglike,
  score = NULL,
  fisher = NULL,
  constraint = NULL,
  theta_names = NULL,
  n_obs = NULL,
  cache_derivatives = FALSE
) {
  stopifnot(is.function(loglike))
  if (!is.null(score)) stopifnot(is.function(score))
  if (!is.null(fisher)) stopifnot(is.function(fisher))
  if (!is.null(constraint)) stopifnot(inherits(constraint, "mle_constraint"))
  if (!is.null(theta_names)) stopifnot(is.character(theta_names))
  if (!is.null(n_obs)) stopifnot(is.numeric(n_obs), n_obs > 0)
  stopifnot(is.logical(cache_derivatives), length(cache_derivatives) == 1)

  # Default constraint: no constraints
  if (is.null(constraint)) {
    constraint <- mle_constraint()
  }

  structure(
    list(
      loglike = loglike,
      .score = score,
      .fisher = fisher,
      constraint = constraint,
      theta_names = theta_names,
      n_obs = n_obs,
      cache_derivatives = cache_derivatives,
      .cache = new.env(parent = emptyenv())
    ),
    class = "mle_problem"
  )
}

#' @export
print.mle_problem <- function(x, ...) {
  cat("MLE Problem\n")
  cat("  Parameters:", if (!is.null(x$theta_names)) paste(x$theta_names, collapse = ", ") else "unnamed", "\n")
  score_type <- if (!is.null(x$.score)) "analytic" else "numerical"
  fisher_type <- if (!is.null(x$.fisher)) "analytic" else "numerical"
  if (isTRUE(x$cache_derivatives)) {
    if (is.null(x$.score)) score_type <- paste0(score_type, " (cached)")
    if (is.null(x$.fisher)) fisher_type <- paste0(fisher_type, " (cached)")
  }
  cat("  Score:", score_type, "\n")
  cat("  Fisher:", fisher_type, "\n")
  cat("  Constraints:", if (!identical(x$constraint, mle_constraint())) "yes" else "none", "\n")
  if (!is.null(x$n_obs)) cat("  Observations:", x$n_obs, "\n")
  invisible(x)
}

# Build a cached numerical derivative function
# @param problem An mle_problem object
# @param compute_fn Function(theta) that computes the derivative
# @param cache_key Prefix for cache entries in problem$.cache
# @return A function(theta) with optional single-value caching
# @noRd
make_cached_derivative <- function(problem, compute_fn, cache_key) {
  theta_key <- paste0(cache_key, "_theta")
  value_key <- paste0(cache_key, "_value")

  function(theta) {
    if (isTRUE(problem$cache_derivatives)) {
      cached_theta <- problem$.cache[[theta_key]]
      if (!is.null(cached_theta) && identical(theta, cached_theta)) {
        return(problem$.cache[[value_key]])
      }
    }

    result <- compute_fn(theta)

    if (isTRUE(problem$cache_derivatives)) {
      problem$.cache[[theta_key]] <- theta
      problem$.cache[[value_key]] <- result
    }

    result
  }
}

#' Get score function from problem
#'
#' Returns the score (gradient) function, computing numerically if not provided.
#' If \code{cache_derivatives = TRUE} was set in the problem and score is
#' computed numerically, results are cached using a single-value cache.
#'
#' @param problem An mle_problem object
#' @return Score function
#' @export
get_score <- function(problem) {
  if (!is.null(problem$.score)) {
    problem$.score
  } else {
    make_cached_derivative(
      problem,
      function(theta) numDeriv::grad(problem$loglike, theta),
      "score"
    )
  }
}

#' Get Fisher information function from problem
#'
#' Returns the Fisher information matrix function, computing numerically if not provided.
#' If \code{cache_derivatives = TRUE} was set in the problem and Fisher is
#' computed numerically, results are cached using a single-value cache.
#'
#' @param problem An mle_problem object
#' @return Fisher information function
#' @export
get_fisher <- function(problem) {
  if (!is.null(problem$.fisher)) {
    problem$.fisher
  } else {
    make_cached_derivative(
      problem,
      function(theta) -numDeriv::hessian(problem$loglike, theta),
      "fisher"
    )
  }
}

#' Check if object is an mle_problem
#'
#' @param x Object to test
#' @return Logical
#' @export
is_mle_problem <- function(x) {
  inherits(x, "mle_problem")
}

#' Clear derivative cache
#'
#' Clears the cached numerical derivatives (score and Fisher) from an mle_problem.
#' This is useful when you want to force recomputation, for example after
#' modifying data that the log-likelihood depends on.
#'
#' @param problem An mle_problem object
#' @return The problem object (invisibly), modified in place
#' @examples
#' \dontrun{
#' problem <- mle_problem(loglike, cache_derivatives = TRUE)
#' # ... run some optimization ...
#' clear_cache(problem)  # Force fresh derivative computation
#' }
#' @export
clear_cache <- function(problem) {
  stopifnot(is_mle_problem(problem))
  rm(list = ls(problem$.cache), envir = problem$.cache)
  invisible(problem)
}

#' Update an mle_problem
#'
#' Create a new problem with some fields updated.
#'
#' @param object An mle_problem
#' @param ... Named arguments to update
#' @return New mle_problem
#' @export
update.mle_problem <- function(object, ...) {
  args <- list(...)
  current <- list(
    loglike = object$loglike,
    score = object$.score,
    fisher = object$.fisher,
    constraint = object$constraint,
    theta_names = object$theta_names,
    n_obs = object$n_obs,
    cache_derivatives = object$cache_derivatives
  )
  current[names(args)] <- args
  do.call(mle_problem, current)
}
