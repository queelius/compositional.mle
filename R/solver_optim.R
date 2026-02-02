# Convert optim() result (which minimizes) to mle_numerical (which maximizes)
.optim_to_mle <- function(optim_result, solver_name, superclass) {
  sol <- list(
    par = optim_result$par,
    value = -optim_result$value,
    convergence = optim_result$convergence,
    hessian = -optim_result$hessian
  )

  mle_result <- algebraic.mle::mle_numerical(
    sol = sol,
    superclasses = superclass
  )

  mle_result$iterations <- optim_result$counts["function"]
  mle_result$solver <- solver_name
  mle_result$optim_result <- optim_result

  mle_result
}

#' BFGS Solver
#'
#' Creates a solver using the BFGS quasi-Newton method via \code{optim()}.
#' BFGS approximates the Hessian from gradient information, providing
#' second-order-like convergence without computing the Hessian directly.
#'
#' @param max_iter Maximum number of iterations
#' @param tol Convergence tolerance (passed to optim's reltol)
#' @param report Reporting frequency (0 = no reporting)
#' @return A solver function with signature (problem, theta0, trace) -> mle_result
#'
#' @details
#' BFGS is often a good default choice: it's more robust than Newton-Raphson
#' (no matrix inversion issues) and faster than gradient ascent (uses
#' curvature information).
#'
#' The solver automatically uses the score function from the problem if
#' available, otherwise computes gradients numerically.
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' x <- rnorm(50, 5, 2)
#' problem <- mle_problem(
#'   loglike = function(theta) sum(dnorm(x, theta[1], theta[2], log = TRUE)),
#'   constraint = mle_constraint(support = function(theta) theta[2] > 0,
#'                               project = function(theta) c(theta[1], max(theta[2], 1e-8)))
#' )
#' # Basic usage
#' result <- bfgs()(problem, c(4, 1.5))
#'
#' # Race BFGS against gradient ascent
#' strategy <- bfgs() %|% gradient_ascent()
#' }
#'
#' @export
bfgs <- function(max_iter = 100L, tol = 1e-8, report = 0L) {
  max_iter <- as.integer(max_iter)
  report <- as.integer(report)

  function(problem, theta0, trace = mle_trace()) {
    stopifnot(is_mle_problem(problem))
    stopifnot(is.numeric(theta0))

    loglike <- problem$loglike
    score_fn <- get_score(problem)
    constraint <- problem$constraint

    if (!constraint$support(theta0)) {
      theta0 <- constraint$project(theta0)
    }

    fn <- function(theta) {
      if (!constraint$support(theta)) return(Inf)
      -loglike(theta)
    }

    gr <- function(theta) {
      if (!constraint$support(theta)) return(rep(NA, length(theta)))
      -score_fn(theta)
    }

    result <- optim(
      par = theta0,
      fn = fn,
      gr = gr,
      method = "BFGS",
      control = list(
        maxit = max_iter,
        reltol = tol,
        trace = report,
        REPORT = if (report > 0) report else 1
      ),
      hessian = TRUE
    )

    .optim_to_mle(result, "bfgs", "mle_bfgs")
  }
}

#' L-BFGS-B Solver (Box Constrained)
#'
#' Creates a solver using L-BFGS-B, a limited-memory BFGS variant that
#' supports box constraints (lower and upper bounds on parameters).
#'
#' @param lower Lower bounds on parameters (can be -Inf)
#' @param upper Upper bounds on parameters (can be Inf)
#' @param max_iter Maximum number of iterations
#' @param tol Convergence tolerance
#' @return A solver function
#'
#' @details
#' Unlike the constraint system in mle_problem (which uses projection),
#' L-BFGS-B handles box constraints natively within the algorithm.
#' Use this when you have simple bound constraints.
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' x <- rnorm(50, 5, 2)
#' problem <- mle_problem(
#'   loglike = function(theta) sum(dnorm(x, theta[1], theta[2], log = TRUE))
#' )
#' # Positive sigma via box constraint
#' solver <- lbfgsb(lower = c(-Inf, 0.01), upper = c(Inf, Inf))
#' result <- solver(problem, c(4, 1.5))
#' }
#'
#' @export
lbfgsb <- function(lower = -Inf, upper = Inf, max_iter = 100L, tol = 1e-8) {
  max_iter <- as.integer(max_iter)

  function(problem, theta0, trace = mle_trace()) {
    stopifnot(is_mle_problem(problem))
    stopifnot(is.numeric(theta0))

    loglike <- problem$loglike
    score_fn <- get_score(problem)

    if (length(lower) == 1) lower <- rep(lower, length(theta0))
    if (length(upper) == 1) upper <- rep(upper, length(theta0))

    fn <- function(theta) -loglike(theta)
    gr <- function(theta) -score_fn(theta)

    result <- optim(
      par = theta0,
      fn = fn,
      gr = gr,
      method = "L-BFGS-B",
      lower = lower,
      upper = upper,
      control = list(maxit = max_iter, factr = tol / .Machine$double.eps),
      hessian = TRUE
    )

    .optim_to_mle(result, "lbfgsb", "mle_lbfgsb")
  }
}

#' Nelder-Mead Solver (Derivative-Free)
#'
#' Creates a solver using the Nelder-Mead simplex method via \code{optim()}.
#' This is a derivative-free method useful when gradients are unavailable
#' or unreliable.
#'
#' @param max_iter Maximum number of iterations
#' @param tol Convergence tolerance
#' @return A solver function
#'
#' @details
#' Nelder-Mead doesn't use gradient information, making it robust but
#' potentially slower. It's useful as a fallback when gradient-based
#' methods fail, or for problems with non-smooth likelihoods.
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' x <- rnorm(50, 5, 2)
#' problem <- mle_problem(
#'   loglike = function(theta) sum(dnorm(x, theta[1], theta[2], log = TRUE)),
#'   constraint = mle_constraint(support = function(theta) theta[2] > 0,
#'                               project = function(theta) c(theta[1], max(theta[2], 1e-8)))
#' )
#' # Use when gradients are problematic
#' result <- nelder_mead()(problem, c(4, 1.5))
#'
#' # Race against gradient methods
#' strategy <- gradient_ascent() %|% nelder_mead()
#' }
#'
#' @export
nelder_mead <- function(max_iter = 500L, tol = 1e-8) {
  max_iter <- as.integer(max_iter)

  function(problem, theta0, trace = mle_trace()) {
    stopifnot(is_mle_problem(problem))
    stopifnot(is.numeric(theta0))

    loglike <- problem$loglike
    constraint <- problem$constraint

    if (!constraint$support(theta0)) {
      theta0 <- constraint$project(theta0)
    }

    fn <- function(theta) {
      if (!constraint$support(theta)) return(Inf)
      -loglike(theta)
    }

    result <- optim(
      par = theta0,
      fn = fn,
      method = "Nelder-Mead",
      control = list(maxit = max_iter, reltol = tol),
      hessian = TRUE
    )

    .optim_to_mle(result, "nelder_mead", "mle_nelder_mead")
  }
}
