#' Gradient Ascent Solver
#'
#' Creates a solver that uses gradient ascent (steepest ascent) to find the MLE.
#' Optionally uses backtracking line search for adaptive step sizes.
#'
#' @param learning_rate Base learning rate / maximum step size
#' @param line_search Use backtracking line search for adaptive step sizes
#' @param max_iter Maximum number of iterations
#' @param tol Convergence tolerance (on parameter change)
#' @param backtrack_ratio Step size reduction factor for line search (0 < r < 1)
#' @param min_step Minimum step size before giving up
#' @param verbose Logical; if TRUE and the \pkg{cli} package is installed,
#'   display progress during optimization. Default is FALSE.
#' @return A solver function with signature (problem, theta0, trace) -> mle_result
#'
#' @details
#' Gradient ascent iteratively moves in the direction of the score (gradient
#' of log-likelihood). With line search enabled, the step size is adaptively
#' chosen to ensure the log-likelihood increases.
#'
#' The solver respects constraints defined in the problem via projection.
#'
#' @examples
#' # Create a solver with default parameters
#' solver <- gradient_ascent()
#'
#' # Create a solver with custom parameters
#' solver <- gradient_ascent(
#'   learning_rate = 0.5,
#'   max_iter = 500,
#'   tol = 1e-10
#' )
#'
#' # Without line search (fixed step size)
#' solver <- gradient_ascent(learning_rate = 0.01, line_search = FALSE)
#'
#' @export
gradient_ascent <- function(
  learning_rate = 1.0,
  line_search = TRUE,
  max_iter = 100L,
  tol = 1e-8,
  backtrack_ratio = 0.5,
  min_step = 1e-12,
  verbose = FALSE
) {
  # Validate parameters
  stopifnot(learning_rate > 0)
  stopifnot(is.logical(line_search))
  stopifnot(max_iter > 0)
  stopifnot(tol > 0)
  stopifnot(backtrack_ratio > 0, backtrack_ratio < 1)
  stopifnot(min_step > 0)
  stopifnot(is.logical(verbose), length(verbose) == 1)

  max_iter <- as.integer(max_iter)

  function(problem, theta0, trace = mle_trace()) {
    stopifnot(is_mle_problem(problem))
    stopifnot(is.numeric(theta0))

    loglike <- problem$loglike
    score <- get_score(problem)
    constraint <- problem$constraint

    theta0 <- .ensure_support(theta0, constraint)
    recorder <- new_trace_recorder(trace, length(theta0))

    progress <- .progress_handler(
      verbose = verbose,
      solver_name = "Gradient Ascent",
      max_iter = max_iter
    )
    progress$start()

    theta <- theta0
    converged <- FALSE

    for (iter in seq_len(max_iter)) {
      grad <- score(theta)
      grad_norm <- sqrt(sum(grad^2))
      ll_current <- loglike(theta)

      if (!is.null(recorder)) {
        record_iteration(recorder, theta,
                        value = ll_current,
                        gradient = grad)
      }

      progress$update(iter, ll_current, grad_norm)

      if (line_search) {
        step_result <- .backtracking_line_search(
          loglike = loglike,
          theta = theta,
          direction = grad,
          max_step = learning_rate,
          backtrack_ratio = backtrack_ratio,
          min_step = min_step,
          constraint = constraint
        )

        if (!step_result$success) {
          if (grad_norm < tol) converged <- TRUE
          break
        }

        theta_new <- step_result$theta
      } else {
        theta_new <- theta + learning_rate * grad
        if (!constraint$support(theta_new)) {
          theta_new <- constraint$project(theta_new)
        }
      }

      if (sqrt(sum((theta_new - theta)^2)) < tol) {
        converged <- TRUE
        theta <- theta_new
        break
      }

      theta <- theta_new
    }

    ll_final <- loglike(theta)
    progress$finish(converged, iter, ll_final)

    fisher <- .numerical_fisher(loglike, theta)
    hessian <- if (!is.null(fisher)) -fisher else NULL

    .build_result(theta, ll_final, converged, hessian,
                  "gradient_ascent", "mle_gradient_ascent", iter, recorder)
  }
}

#' Backtracking line search
#'
#' @keywords internal
.backtracking_line_search <- function(
  loglike,
  theta,
  direction,
  max_step,
  backtrack_ratio,
  min_step,
  constraint
) {
  dir_norm <- sqrt(sum(direction^2))
  if (dir_norm == 0) {
    return(list(success = FALSE, theta = theta))
  }

  ll_current <- loglike(theta)
  step_size <- max_step / dir_norm

  while (step_size >= min_step) {
    theta_new <- theta + step_size * direction

    # Project if needed
    if (!constraint$support(theta_new)) {
      theta_new <- constraint$project(theta_new)
    }

    if (constraint$support(theta_new)) {
      ll_new <- loglike(theta_new)

      if (!is.na(ll_new) && !is.nan(ll_new) && ll_new > ll_current) {
        return(list(success = TRUE, theta = theta_new, step_size = step_size))
      }
    }

    step_size <- step_size * backtrack_ratio
  }

  list(success = FALSE, theta = theta)
}
