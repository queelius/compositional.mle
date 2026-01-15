#' Coordinate Ascent Solver
#'
#' Creates a solver that optimizes one parameter at a time while holding
#' others fixed. This is useful when parameters have different scales or
#' when the likelihood decomposes nicely along coordinate directions.
#'
#' @param max_cycles Maximum number of full cycles through all parameters
#' @param tol Convergence tolerance on log-likelihood change
#' @param line_search Use line search for each coordinate (slower but more robust)
#' @param cycle_order Order of cycling: "sequential" (1,2,...,p) or "random"
#' @return A solver function with signature (problem, theta0, trace) -> mle_result
#'
#' @details
#' Each cycle consists of optimizing each coordinate in turn using a simple
#' golden section search. The algorithm converges when the log-likelihood
#' improvement in a full cycle is less than \code{tol}.
#'
#' Coordinate ascent can be effective when:
#' \itemize{
#'   \item Parameters are on very different scales
#'   \item The likelihood has axis-aligned ridges
#'   \item Computing the full gradient is expensive
#' }
#'
#' However, it may converge slowly for problems with strong parameter correlations.
#'
#' @examples
#' # Basic coordinate ascent
#' solver <- coordinate_ascent()
#'
#' # With more cycles for difficult problems
#' solver <- coordinate_ascent(max_cycles = 100)
#'
#' # Random cycling to avoid systematic bias
#' solver <- coordinate_ascent(cycle_order = "random")
#'
#' @export
coordinate_ascent <- function(
  max_cycles = 50L,
  tol = 1e-8,
  line_search = TRUE,
  cycle_order = c("sequential", "random")
) {
  # Validate parameters
  stopifnot(max_cycles > 0)
  stopifnot(tol > 0)
  stopifnot(is.logical(line_search))

  max_cycles <- as.integer(max_cycles)
  cycle_order <- match.arg(cycle_order)

  # Return solver function
  function(problem, theta0, trace = mle_trace()) {
    stopifnot(is_mle_problem(problem))
    stopifnot(is.numeric(theta0))

    loglike <- problem$loglike
    constraint <- problem$constraint
    n_params <- length(theta0)

    # Check initial point is in support
    if (!constraint$support(theta0)) {
      theta0 <- constraint$project(theta0)
      if (!constraint$support(theta0)) {
        stop("Initial point not in support and projection failed")
      }
    }

    # Initialize tracing
    recorder <- new_trace_recorder(trace, n_params)

    # Initialize state
    theta <- theta0
    ll_current <- loglike(theta)

    if (!is.finite(ll_current)) {
      stop("Initial log-likelihood is not finite")
    }

    converged <- FALSE
    total_iter <- 0L

    # Coordinate ascent cycles
    for (cycle in seq_len(max_cycles)) {
      ll_start <- ll_current

      # Determine coordinate order for this cycle
      coord_order <- if (cycle_order == "random") {
        sample(n_params)
      } else {
        seq_len(n_params)
      }

      # Optimize each coordinate
      for (j in coord_order) {
        total_iter <- total_iter + 1L

        # Record iteration
        if (!is.null(recorder)) {
          record_iteration(recorder, theta, value = ll_current)
        }

        if (line_search) {
          # Golden section search along coordinate j
          result_j <- .coordinate_line_search(
            loglike = loglike,
            theta = theta,
            coord = j,
            constraint = constraint
          )
          theta <- result_j$theta
          ll_current <- result_j$value
        } else {
          # Simple step in gradient direction for coordinate j
          # Use numerical derivative for this coordinate
          eps <- sqrt(.Machine$double.eps)
          theta_plus <- theta
          theta_plus[j] <- theta[j] + eps
          theta_minus <- theta
          theta_minus[j] <- theta[j] - eps

          # Central difference
          ll_plus <- if (constraint$support(theta_plus)) loglike(theta_plus) else -Inf
          ll_minus <- if (constraint$support(theta_minus)) loglike(theta_minus) else -Inf

          if (is.finite(ll_plus) && is.finite(ll_minus)) {
            grad_j <- (ll_plus - ll_minus) / (2 * eps)

            # Take a step
            step <- 0.1 * sign(grad_j)
            theta_new <- theta
            theta_new[j] <- theta[j] + step

            if (constraint$support(theta_new)) {
              ll_new <- loglike(theta_new)
              if (is.finite(ll_new) && ll_new > ll_current) {
                theta <- theta_new
                ll_current <- ll_new
              }
            }
          }
        }
      }

      # Check convergence
      ll_improvement <- ll_current - ll_start
      if (abs(ll_improvement) < tol) {
        converged <- TRUE
        break
      }
    }

    # Compute Fisher information numerically
    fisher <- tryCatch(
      -numDeriv::hessian(loglike, theta),
      error = function(e) NULL
    )

    # Build result
    sol <- list(
      par = theta,
      value = ll_current,
      convergence = if (converged) 0L else 1L,
      hessian = if (!is.null(fisher)) -fisher else NULL
    )

    result <- algebraic.mle::mle_numerical(
      sol = sol,
      superclasses = "mle_coordinate_ascent"
    )

    result$iterations <- total_iter
    result$solver <- "coordinate_ascent"
    result$trace_data <- finalize_trace(recorder)
    result$cycles <- cycle

    result
  }
}


#' Golden section line search along one coordinate
#'
#' @param loglike Log-likelihood function
#' @param theta Current parameter vector
#' @param coord Index of coordinate to optimize
#' @param constraint Constraint object
#' @keywords internal
.coordinate_line_search <- function(loglike, theta, coord, constraint,
                                     max_iter = 50, tol = 1e-8) {
  # Wrapper that evaluates loglike at theta with coord set to x
  f <- function(x) {
    theta_test <- theta
    theta_test[coord] <- x
    if (!constraint$support(theta_test)) return(-Inf)
    ll <- tryCatch(loglike(theta_test), error = function(e) -Inf)
    ll
  }

  current_val <- theta[coord]
  current_ll <- f(current_val)

  # Find bracket by expanding search
  # Start with a unit interval around current value
  a <- current_val - 1
  b <- current_val + 1

  # Expand until we have a proper bracket or hit limits
  while (f(a) > current_ll && abs(a) < 1000) {
    a <- a - 1
  }
  while (f(b) > current_ll && abs(b) < 1000) {
    b <- b + 1
  }

  # Golden ratio
  phi <- (1 + sqrt(5)) / 2
  resphi <- 2 - phi

  # Initial interior points
  x1 <- a + resphi * (b - a)
  x2 <- b - resphi * (b - a)
  f1 <- f(x1)
  f2 <- f(x2)

  # Golden section search to maximize
  for (i in seq_len(max_iter)) {
    if (abs(b - a) < tol) break

    if (f1 > f2) {
      b <- x2
      x2 <- x1
      f2 <- f1
      x1 <- a + resphi * (b - a)
      f1 <- f(x1)
    } else {
      a <- x1
      x1 <- x2
      f1 <- f2
      x2 <- b - resphi * (b - a)
      f2 <- f(x2)
    }
  }

  # Return best point
  best_x <- if (f1 > f2) x1 else x2
  best_ll <- max(f1, f2)

  # Only update if we improved
  if (best_ll > current_ll) {
    theta[coord] <- best_x
    list(theta = theta, value = best_ll)
  } else {
    list(theta = theta, value = current_ll)
  }
}
