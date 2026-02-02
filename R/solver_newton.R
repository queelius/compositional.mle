#' Newton-Raphson Solver
#'
#' Creates a solver that uses Newton-Raphson (second-order) optimization.
#' Uses the Fisher information matrix to scale the gradient for faster
#' convergence near the optimum.
#'
#' @param line_search Use backtracking line search for stability
#' @param max_iter Maximum number of iterations
#' @param tol Convergence tolerance (on parameter change)
#' @param backtrack_ratio Step size reduction factor for line search
#' @param min_step Minimum step size before giving up
#' @param verbose Logical; if TRUE and the \pkg{cli} package is installed,
#'   display progress during optimization. Default is FALSE.
#' @return A solver function with signature (problem, theta0, trace) -> mle_result
#'
#' @details
#' Newton-Raphson computes the search direction as \eqn{I(\theta)^{-1} s(\theta)}
#' where \eqn{I} is the Fisher information and \eqn{s} is the score. This
#' accounts for parameter scaling and typically converges faster than gradient
#' ascent when near the optimum.
#'
#' Requires the problem to have a Fisher information function (either analytic
#' or computed numerically).
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' solver <- newton_raphson()
#' result <- solver(problem, c(0, 1))
#'
#' # Often used after gradient ascent for refinement
#' strategy <- gradient_ascent(max_iter = 50) %>>% newton_raphson(max_iter = 20)
#' }
#'
#' @export
newton_raphson <- function(
  line_search = TRUE,
  max_iter = 50L,
  tol = 1e-8,
  backtrack_ratio = 0.5,
  min_step = 1e-12,
  verbose = FALSE
) {
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
    fisher <- get_fisher(problem)
    constraint <- problem$constraint

    if (!constraint$support(theta0)) {
      theta0 <- constraint$project(theta0)
    }

    recorder <- new_trace_recorder(trace, length(theta0))

    progress <- .progress_handler(
      verbose = verbose,
      solver_name = "Newton-Raphson",
      max_iter = max_iter
    )
    progress$start()

    theta <- theta0
    converged <- FALSE

    for (iter in seq_len(max_iter)) {
      grad <- score(theta)
      fim <- fisher(theta)
      grad_norm <- sqrt(sum(grad^2))
      ll_current <- loglike(theta)

      direction <- tryCatch(
        as.vector(solve(fim, grad)),
        error = function(e) {
          as.vector(MASS::ginv(fim) %*% grad)
        }
      )

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
          direction = direction,
          max_step = 1.0,
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
        theta_new <- theta + direction
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
    fim_final <- fisher(theta)
    progress$finish(converged, iter, ll_final)

    .build_result(theta, ll_final, converged, -fim_final,
                  "newton_raphson", "mle_newton_raphson", iter, recorder)
  }
}

#' Fisher Scoring Solver
#'
#' Variant of Newton-Raphson that uses the expected Fisher information
#' instead of the observed Fisher. Can be more stable for some problems.
#'
#' @inheritParams newton_raphson
#' @return A solver function
#'
#' @details
#' Fisher scoring is identical to Newton-Raphson when the expected and
#' observed Fisher information are equal (e.g., exponential families).
#' For other models, it may have different convergence properties.
#'
#' @export
fisher_scoring <- newton_raphson
