#' Simulated Annealing Solver
#'
#' Creates a solver using simulated annealing for global optimization.
#' Simulated annealing can escape local optima by probabilistically accepting
#' worse solutions, with the acceptance probability decreasing over time
#' (controlled by a "temperature" parameter).
#'
#' @param temp_init Initial temperature (higher = more exploration)
#' @param cooling_rate Temperature reduction factor per iteration (0 < r < 1)
#' @param max_iter Maximum number of iterations
#' @param neighbor_sd Standard deviation for generating neighbor proposals
#' @param min_temp Minimum temperature before stopping
#' @param verbose Logical; if TRUE and the \pkg{cli} package is installed,
#'   display progress during optimization. Default is FALSE.
#' @return A solver function with signature (problem, theta0, trace) -> mle_result
#'
#' @details
#' At each iteration:
#' 1. Generate a neighbor by adding Gaussian noise to current parameters
#' 2. If the neighbor improves the objective, accept it
#' 3. If the neighbor is worse, accept with probability exp(delta / temp)
#' 4. Reduce temperature: temp = temp * cooling_rate
#'
#' The algorithm is stochastic and may find different solutions on different runs.
#' For best results, use with \code{with_restarts()} or combine with a local
#' optimizer via \code{\%>>\%}.
#'
#' @examples
#' # Basic simulated annealing
#' solver <- sim_anneal()
#'
#' # More exploration (higher initial temp, slower cooling)
#' solver <- sim_anneal(temp_init = 100, cooling_rate = 0.999)
#'
#' # Coarse global search, then local refinement
#' strategy <- sim_anneal(max_iter = 500) %>>% gradient_ascent()
#'
#' @export
sim_anneal <- function(
  temp_init = 10.0,
  cooling_rate = 0.95,
  max_iter = 1000L,
  neighbor_sd = 1.0,
  min_temp = 1e-10,
  verbose = FALSE
) {
  # Validate parameters
  stopifnot(temp_init > 0)
  stopifnot(cooling_rate > 0, cooling_rate < 1)
  stopifnot(max_iter > 0)
  stopifnot(neighbor_sd > 0)
  stopifnot(min_temp >= 0)
  stopifnot(is.logical(verbose), length(verbose) == 1)

  max_iter <- as.integer(max_iter)

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

    # Initialize progress handler
    progress <- .progress_handler(
      verbose = verbose,
      solver_name = "Simulated Annealing",
      max_iter = max_iter,
      show_every = max(1L, max_iter %/% 100L)  # Show ~100 updates max
    )
    progress$start()

    # Initialize state
    theta <- theta0
    ll_current <- loglike(theta)

    if (!is.finite(ll_current)) {
      stop("Initial log-likelihood is not finite")
    }

    best_theta <- theta
    best_ll <- ll_current
    temp <- temp_init
    n_accepted <- 0L

    # Simulated annealing loop
    for (iter in seq_len(max_iter)) {
      # Record current state
      if (!is.null(recorder)) {
        record_iteration(recorder, theta, value = ll_current)
      }

      # Update progress (show best LL found so far)
      progress$update(iter, best_ll)

      # Check temperature
      if (temp < min_temp) break

      # Generate neighbor
      theta_new <- theta + rnorm(n_params, mean = 0, sd = neighbor_sd)

      # Apply constraints
      if (!constraint$support(theta_new)) {
        theta_new <- constraint$project(theta_new)
      }

      # Skip if still outside support
      if (!constraint$support(theta_new)) {
        temp <- temp * cooling_rate
        next
      }

      # Evaluate neighbor
      ll_new <- tryCatch(loglike(theta_new), error = function(e) -Inf)

      if (!is.finite(ll_new)) {
        temp <- temp * cooling_rate
        next
      }

      # Decide whether to accept
      delta <- ll_new - ll_current

      accept <- FALSE
      if (delta > 0) {
        # Better solution - always accept
        accept <- TRUE
      } else {
        # Worse solution - accept with probability exp(delta / temp)
        if (runif(1) < exp(delta / temp)) {
          accept <- TRUE
        }
      }

      if (accept) {
        theta <- theta_new
        ll_current <- ll_new
        n_accepted <- n_accepted + 1L

        # Track best solution
        if (ll_current > best_ll) {
          best_theta <- theta
          best_ll <- ll_current
        }
      }

      # Cool down
      temp <- temp * cooling_rate
    }

    # Report completion (SA doesn't have traditional convergence)
    progress$finish(TRUE, iter, best_ll)

    # Compute Fisher information numerically at best point
    fisher <- tryCatch(
      -numDeriv::hessian(loglike, best_theta),
      error = function(e) NULL
    )

    # Build result
    sol <- list(
      par = best_theta,
      value = best_ll,
      convergence = 0L,  # SA doesn't have a convergence criterion
      hessian = if (!is.null(fisher)) -fisher else NULL
    )

    result <- algebraic.mle::mle_numerical(
      sol = sol,
      superclasses = "mle_sim_anneal"
    )

    result$iterations <- iter
    result$solver <- "sim_anneal"
    result$trace_data <- finalize_trace(recorder)
    result$final_temp <- temp
    result$acceptance_rate <- n_accepted / iter

    result
  }
}
