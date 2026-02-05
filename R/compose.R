#' Compose Multiple Solvers Sequentially
#'
#' Chains any number of solvers sequentially. Each solver's result becomes
#' the starting point for the next. Alternative to using \code{\%>>\%} operator.
#'
#' Trace data from all solvers is merged into a single trace with stage
#' boundaries preserved.
#'
#' @param ... Solver functions to compose
#' @return A new solver function that runs all solvers in sequence
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
#' # Three-stage strategy
#' strategy <- compose(
#'   grid_search(lower = c(-10, 0.1), upper = c(10, 5), n = 5),
#'   gradient_ascent(max_iter = 50),
#'   newton_raphson(max_iter = 20)
#' )
#' result <- strategy(problem, c(0, 1))
#' }
#'
#' @export
compose <- function(...) {
  solvers <- list(...)
  stopifnot(length(solvers) >= 1)
  for (s in solvers) stopifnot(is.function(s))

  if (length(solvers) == 1) return(solvers[[1]])

  function(problem, theta0, trace = mle_trace()) {
    result <- solvers[[1]](problem, theta0, trace)
    chain <- list(result)

    for (i in seq_len(length(solvers) - 1) + 1) {
      result <- solvers[[i]](problem, result$theta.hat, trace)
      chain <- c(chain, list(result))
    }

    result$chain <- chain
    result$strategy <- "sequential"
    result$trace_data <- merge_traces(chain)

    result
  }
}

#' Sequential Solver Composition
#'
#' Chains two solvers sequentially. The result of the first solver becomes
#' the starting point for the second. This enables coarse-to-fine strategies.
#'
#' Trace data from all solvers in the chain is merged into a single trace
#' with stage boundaries preserved.
#'
#' @param s1 First solver function
#' @param s2 Second solver function
#' @return A new solver function that runs s1 then s2
#'
#' @examples
#' # Coarse-to-fine: grid search to find good region, then gradient ascent
#' strategy <- grid_search(lower = c(-10, 0.1), upper = c(10, 5), n = 5) %>>%
#'   gradient_ascent()
#'
#' # Three-stage refinement
#' strategy <- grid_search(lower = c(-10, 0.1), upper = c(10, 5), n = 3) %>>%
#'   gradient_ascent() %>>%
#'   newton_raphson()
#'
#' @export
`%>>%` <- function(s1, s2) {
  stopifnot(is.function(s1), is.function(s2))

  function(problem, theta0, trace = mle_trace()) {
    result1 <- s1(problem, theta0, trace)
    result2 <- s2(problem, result1$theta.hat, trace)

    chain <- c(
      if (!is.null(result1$chain)) result1$chain else list(result1),
      list(result2)
    )
    result2$chain <- chain
    result2$strategy <- "sequential"

    result2$trace_data <- merge_traces(chain)
    result2
  }
}

#' Race Multiple Solvers
#'
#' Runs multiple solvers (optionally in parallel) and returns the best result
#' (highest log-likelihood). More flexible than \code{\%|\%} operator.
#'
#' @param ... Solver functions to race
#' @param parallel Logical; if TRUE and the \pkg{future} package is installed,
#'   solvers are run in parallel using the current future plan. Default is FALSE.
#' @return A new solver function that races all solvers and picks the best
#'
#' @details
#' When \code{parallel = TRUE}, solvers are executed using \code{future::future()}
#' and results collected with \code{future::value()}. The current future plan
#' determines how parallelization happens (e.g., \code{plan(multisession)} for
#' multi-process execution).
#'
#' Failed solvers (those that throw errors) are ignored. If all solvers fail,
#' an error is thrown.
#'
#' @examples
#' # Race three methods sequentially
#' strategy <- race(gradient_ascent(), bfgs(), nelder_mead())
#'
#' # Race with parallel execution (requires future package)
#' \dontrun{
#' future::plan(future::multisession)
#' strategy <- race(gradient_ascent(), bfgs(), nelder_mead(), parallel = TRUE)
#' }
#'
#' @export
race <- function(..., parallel = FALSE) {
  solvers <- list(...)
  stopifnot(length(solvers) >= 2)
  for (s in solvers) stopifnot(is.function(s))
  stopifnot(is.logical(parallel), length(parallel) == 1)

  function(problem, theta0, trace = mle_trace()) {
    results <- vector("list", length(solvers))

    if (parallel && requireNamespace("future", quietly = TRUE)) {
      futures <- lapply(solvers, function(s) {
        future::future(
          tryCatch(s(problem, theta0, trace), error = function(e) NULL),
          seed = TRUE
        )
      })
      results <- lapply(futures, future::value)
    } else {
      for (i in seq_along(solvers)) {
        results[[i]] <- tryCatch(
          solvers[[i]](problem, theta0, trace),
          error = function(e) NULL
        )
      }
    }

    loglikes <- sapply(results, function(r) {
      if (!is.null(r) && !is.null(r$loglike)) r$loglike else -Inf
    })

    if (all(loglikes == -Inf)) {
      stop("All solvers in race failed")
    }

    best_idx <- which.max(loglikes)
    winner <- results[[best_idx]]
    winner$alternatives <- results
    winner$strategy <- "race"
    winner$winner_index <- best_idx

    winner
  }
}

#' Parallel Solver Racing (Operator)
#'
#' Runs multiple solvers and returns the best result (highest log-likelihood).
#' Useful when unsure which method will work best for a given problem.
#'
#' For parallel execution or more than 2 solvers, use \code{\link{race}}.
#'
#' @param s1 First solver function
#' @param s2 Second solver function
#' @return A new solver function that runs both and picks the best
#'
#' @examples
#' # Race gradient-based vs derivative-free
#' strategy <- gradient_ascent() %|% nelder_mead()
#'
#' # Race multiple methods
#' strategy <- gradient_ascent() %|% bfgs() %|% nelder_mead()
#'
#' @seealso \code{\link{race}} for parallel execution
#' @name race_operator
#' @rdname race_operator
#' @export
`%|%` <- function(s1, s2) {
  stopifnot(is.function(s1), is.function(s2))

  function(problem, theta0, trace = mle_trace()) {
    result1 <- tryCatch(s1(problem, theta0, trace), error = function(e) NULL)
    result2 <- tryCatch(s2(problem, theta0, trace), error = function(e) NULL)

    if (is.null(result1) && is.null(result2)) {
      stop("All solvers in parallel race failed")
    }
    if (is.null(result1)) return(result2)
    if (is.null(result2)) return(result1)

    ll1 <- if (!is.null(result1$loglike)) result1$loglike else -Inf
    ll2 <- if (!is.null(result2$loglike)) result2$loglike else -Inf

    winner <- if (ll1 >= ll2) result1 else result2
    winner$alternatives <- list(result1, result2)
    winner$strategy <- "race"

    winner
  }
}

#' Multiple Random Restarts
#'
#' Runs a solver from multiple starting points and returns the best result.
#' Essential for problems with multiple local optima.
#'
#' The sampler generates candidate starting points, which are automatically
#' filtered/projected using the problem's constraint. This means samplers
#' can be simple distributions without constraint awareness.
#'
#' @param solver A solver function
#' @param n Number of restarts (including the provided theta0)
#' @param sampler Function that generates random starting points.
#'   Called with no arguments, should return a parameter vector.
#'   Samples are automatically constrained using problem$constraint.
#' @param max_reject Maximum rejection attempts per sample before projection
#' @return A new solver function with restart capability
#'
#' @examples
#' # 20 random restarts - constraint applied automatically from problem
#' sampler <- uniform_sampler(c(-10, 0), c(10, 5))
#' strategy <- with_restarts(gradient_ascent(), n = 20, sampler = sampler)
#'
#' # Can also compose with other operators
#' strategy <- with_restarts(gradient_ascent(), n = 10, sampler = sampler) %>>%
#'   newton_raphson()
#'
#' @export
with_restarts <- function(solver, n, sampler, max_reject = 100L) {
  stopifnot(is.function(solver))
  stopifnot(is.numeric(n), n >= 1)
  stopifnot(is.function(sampler))

  n <- as.integer(n)
  max_reject <- as.integer(max_reject)

  function(problem, theta0, trace = mle_trace()) {
    constraint <- problem$constraint

    sample_valid <- function() {
      for (attempt in seq_len(max_reject)) {
        theta <- sampler()
        if (constraint$support(theta)) return(theta)
      }
      constraint$project(sampler())
    }

    starts <- vector("list", n)
    starts[[1]] <- theta0
    for (i in seq_len(n - 1) + 1) {
      starts[[i]] <- sample_valid()
    }

    results <- vector("list", n)
    loglikes <- rep(-Inf, n)

    for (i in seq_len(n)) {
      results[[i]] <- tryCatch(
        solver(problem, starts[[i]], trace),
        error = function(e) NULL
      )
      if (!is.null(results[[i]]) && !is.null(results[[i]]$loglike)) {
        loglikes[i] <- results[[i]]$loglike
      }
    }

    if (all(loglikes == -Inf)) {
      stop("All restart attempts failed")
    }

    best_idx <- which.max(loglikes)
    best <- results[[best_idx]]

    best$n_restarts <- n
    best$restart_loglikes <- loglikes
    best$best_restart <- best_idx
    best$strategy <- "restarts"

    best
  }
}

#' Conditional Refinement
#'
#' Applies a refinement solver only if the first solver did not converge.
#' If refinement is applied, trace data from both solvers is merged.
#'
#' @param solver Primary solver function
#' @param refinement Solver to use if primary doesn't converge
#' @return A new solver function with conditional refinement
#'
#' @examples
#' # Use Newton-Raphson to refine if gradient ascent doesn't converge
#' strategy <- unless_converged(gradient_ascent(max_iter = 50), newton_raphson())
#'
#' @export
unless_converged <- function(solver, refinement) {
  stopifnot(is.function(solver), is.function(refinement))

  function(problem, theta0, trace = mle_trace()) {
    result <- solver(problem, theta0, trace)

    if (!isTRUE(result$converged)) {
      result2 <- refinement(problem, result$theta.hat, trace)
      result2$chain <- list(result, result2)
      result2$strategy <- "conditional"
      result2$trace_data <- merge_traces(result2$chain)
      return(result2)
    }

    result
  }
}

#' Chain Solvers with Early Stopping
#'
#' Chains multiple solvers sequentially with optional early stopping.
#' More flexible than \code{\%>>\%} operator.
#'
#' @param ... Solver functions to chain
#' @param early_stop Optional function that takes a result and returns TRUE
#'   to stop the chain early. Default is NULL (no early stopping).
#' @return A new solver function that runs solvers in sequence
#'
#' @details
#' The chain runs solvers in order, passing each result's \code{theta.hat}
#' to the next solver. If \code{early_stop} is provided and returns TRUE
#' for any intermediate result, the chain stops early.
#'
#' Common early stopping conditions:
#' \itemize{
#'   \item Stop when converged: \code{function(r) r$converged}
#'   \item Stop when gradient is small: \code{function(r) sqrt(sum(score^2)) < 1e-6}
#'   \item Stop after reaching target: \code{function(r) r$loglike > -100}
#' }
#'
#' @examples
#' # Chain with early stopping when converged
#' strategy <- chain(
#'   grid_search(lower = c(-10, 0.1), upper = c(10, 5), n = 5),
#'   gradient_ascent(max_iter = 50),
#'   newton_raphson(max_iter = 20),
#'   early_stop = function(r) isTRUE(r$converged)
#' )
#'
#' # Standard chain (no early stopping)
#' strategy <- chain(gradient_ascent(), newton_raphson())
#'
#' @export
chain <- function(..., early_stop = NULL) {
  solvers <- list(...)
  stopifnot(length(solvers) >= 1)
  for (s in solvers) stopifnot(is.function(s))
  if (!is.null(early_stop)) stopifnot(is.function(early_stop))

  if (length(solvers) == 1 && is.null(early_stop)) return(solvers[[1]])

  function(problem, theta0, trace = mle_trace()) {
    finalize <- function(result, chain_results, stopped_early) {
      result$chain <- chain_results
      result$strategy <- "chain"
      result$stopped_early <- stopped_early
      result$trace_data <- merge_traces(chain_results)
      result
    }

    result <- solvers[[1]](problem, theta0, trace)
    chain_results <- list(result)

    for (i in seq_len(length(solvers) - 1) + 1) {
      if (!is.null(early_stop) && early_stop(result)) {
        return(finalize(result, chain_results, stopped_early = TRUE))
      }
      result <- solvers[[i]](problem, result$theta.hat, trace)
      chain_results <- c(chain_results, list(result))
    }

    if (!is.null(early_stop) && early_stop(result)) {
      return(finalize(result, chain_results, stopped_early = TRUE))
    }

    finalize(result, chain_results, stopped_early = FALSE)
  }
}

#' Uniform Sampler Factory
#'
#' Creates a sampler function for use with \code{with_restarts} that
#' generates uniformly distributed starting points.
#'
#' @param lower Lower bounds for each parameter
#' @param upper Upper bounds for each parameter
#' @return A sampler function
#'
#' @examples
#' sampler <- uniform_sampler(c(-10, 0.1), c(10, 5))
#' strategy <- with_restarts(gradient_ascent(), n = 20, sampler = sampler)
#'
#' @export
uniform_sampler <- function(lower, upper) {
  stopifnot(length(lower) == length(upper))
  stopifnot(all(lower <= upper))

  function() {
    runif(length(lower), min = lower, max = upper)
  }
}

#' Normal Sampler Factory
#'
#' Creates a sampler function for use with \code{with_restarts} that
#' generates normally distributed starting points around a center.
#'
#' @param center Mean of the normal distribution
#' @param sd Standard deviation (scalar or vector)
#' @return A sampler function
#'
#' @examples
#' sampler <- normal_sampler(c(0, 1), sd = c(5, 0.5))
#' strategy <- with_restarts(gradient_ascent(), n = 20, sampler = sampler)
#'
#' @export
normal_sampler <- function(center, sd = 1) {
  if (length(sd) == 1) sd <- rep(sd, length(center))
  stopifnot(length(center) == length(sd))

  function() {
    rnorm(length(center), mean = center, sd = sd)
  }
}
