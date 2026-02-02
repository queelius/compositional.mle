#' Grid Search Solver
#'
#' Creates a solver that evaluates the log-likelihood on a grid of points
#' and returns the best. Useful for finding good starting points or for
#' low-dimensional problems.
#'
#' @param lower Lower bounds for the grid
#' @param upper Upper bounds for the grid
#' @param n Number of points per dimension (scalar or vector)
#' @return A solver function with signature (problem, theta0, trace) -> mle_result
#'
#' @details
#' Grid search is deterministic and exhaustive within its bounds. It's most
#' useful for 1-3 dimensional problems or as the first stage of a multi-stage
#' strategy (e.g., grid_search %>>% gradient_ascent).
#'
#' The theta0 argument is ignored; the grid is determined by lower/upper/n.
#' Points outside the problem's constraint support are skipped.
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
#' # Simple grid search
#' solver <- grid_search(lower = c(-10, 0.1), upper = c(10, 5), n = 20)
#' result <- solver(problem, c(0, 1))
#'
#' # Coarse-to-fine: grid then gradient
#' strategy <- grid_search(c(-10, 0.1), c(10, 5), n = 5) %>>% gradient_ascent()
#' }
#'
#' @export
grid_search <- function(lower, upper, n = 10L) {
  stopifnot(is.numeric(lower), is.numeric(upper))
  stopifnot(length(lower) == length(upper))
  stopifnot(all(lower <= upper))

  if (length(n) == 1) n <- rep(as.integer(n), length(lower))
  stopifnot(length(n) == length(lower))
  stopifnot(all(n >= 1))

  function(problem, theta0, trace = mle_trace()) {
    stopifnot(is_mle_problem(problem))

    loglike <- problem$loglike
    constraint <- problem$constraint

    # Generate grid
    grid_list <- lapply(seq_along(lower), function(i) {
      seq(lower[i], upper[i], length.out = n[i])
    })
    grid <- as.matrix(expand.grid(grid_list))

    # Evaluate at each grid point
    best_theta <- NULL
    best_ll <- -Inf
    n_evaluated <- 0L

    for (i in seq_len(nrow(grid))) {
      theta <- grid[i, ]

      if (!constraint$support(theta)) next

      ll <- tryCatch(
        loglike(theta),
        error = function(e) -Inf
      )

      if (is.finite(ll)) {
        n_evaluated <- n_evaluated + 1L
        if (ll > best_ll) {
          best_ll <- ll
          best_theta <- theta
        }
      }
    }

    if (is.null(best_theta)) {
      stop("Grid search found no valid points")
    }

    # Build result
    sol <- list(
      par = best_theta,
      value = best_ll,
      convergence = 0L,
      hessian = NULL
    )

    result <- algebraic.mle::mle_numerical(
      sol = sol,
      superclasses = "mle_grid_search"
    )

    result$iterations <- n_evaluated
    result$solver <- "grid_search"
    result$grid_points <- nrow(grid)
    result$grid_evaluated <- n_evaluated

    result
  }
}

#' Random Search Solver
#'
#' Creates a solver that evaluates the log-likelihood at random points
#' and returns the best. Useful for high-dimensional problems where
#' grid search is infeasible.
#'
#' @param sampler Function generating random parameter vectors
#' @param n Number of random points to evaluate
#' @return A solver function
#'
#' @details
#' Unlike grid search, random search scales better to high dimensions.
#' The sampler should generate points in a reasonable region; points
#' outside the problem's constraint support are skipped.
#'
#' @examples
#' # Create a random search solver with uniform sampling
#' solver <- random_search(
#'   sampler = uniform_sampler(c(-10, 0.1), c(10, 5)),
#'   n = 100
#' )
#'
#' @export
random_search <- function(sampler, n = 100L) {
  stopifnot(is.function(sampler))
  n <- as.integer(n)
  stopifnot(n >= 1)

  function(problem, theta0, trace = mle_trace()) {
    stopifnot(is_mle_problem(problem))

    loglike <- problem$loglike
    constraint <- problem$constraint

    best_theta <- NULL
    best_ll <- -Inf
    n_evaluated <- 0L

    # Always include theta0
    if (constraint$support(theta0)) {
      ll0 <- tryCatch(loglike(theta0), error = function(e) -Inf)
      if (is.finite(ll0)) {
        best_theta <- theta0
        best_ll <- ll0
        n_evaluated <- 1L
      }
    }

    # Random samples
    for (i in seq_len(n - 1)) {
      theta <- sampler()

      # Apply constraint (with rejection/projection)
      if (!constraint$support(theta)) {
        theta <- constraint$project(theta)
      }

      if (!constraint$support(theta)) next

      ll <- tryCatch(loglike(theta), error = function(e) -Inf)

      if (is.finite(ll)) {
        n_evaluated <- n_evaluated + 1L
        if (ll > best_ll) {
          best_ll <- ll
          best_theta <- theta
        }
      }
    }

    if (is.null(best_theta)) {
      stop("Random search found no valid points")
    }

    sol <- list(
      par = best_theta,
      value = best_ll,
      convergence = 0L,
      hessian = NULL
    )

    result <- algebraic.mle::mle_numerical(
      sol = sol,
      superclasses = "mle_random_search"
    )

    result$iterations <- n_evaluated
    result$solver <- "random_search"
    result$n_samples <- n
    result$n_evaluated <- n_evaluated

    result
  }
}
