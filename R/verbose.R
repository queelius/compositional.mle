#' Verbose Output Utilities
#'
#' Internal functions for progress reporting during optimization.
#'
#' @name verbose-utils
#' @keywords internal
NULL

#' Check if cli package is available
#' @keywords internal
.has_cli <- function() {
  requireNamespace("cli", quietly = TRUE)
}

#' Create a progress handler for optimization
#'
#' @param verbose Logical; whether to show progress
#' @param solver_name Name of the solver for display
#' @param max_iter Maximum iterations (for progress bar)
#' @param show_every Show progress every N iterations
#' @return A list with start(), update(), and finish() functions
#' @keywords internal
.progress_handler <- function(verbose = FALSE, solver_name = "Solver",
                              max_iter = NULL, show_every = 1L) {
  if (!verbose) {
    # Return no-op functions
    return(list(
      start = function() invisible(NULL),
      update = function(...) invisible(NULL),
      finish = function(...) invisible(NULL)
    ))
  }

  # Track state
  last_ll <- -Inf
  start_time <- NULL
  pb <- NULL

  start <- function() {
    start_time <<- Sys.time()
    if (.has_cli() && !is.null(max_iter)) {
      # Use cli progress bar
      cli::cli_progress_bar(
        name = solver_name,
        total = max_iter,
        format = paste0(
          "{cli::pb_name} | {cli::pb_current}/{cli::pb_total} | ",
          "LL: {.val {format(ll, digits = 4, nsmall = 2)}} | ",
          "|grad|: {.val {format(grad_norm, digits = 2, scientific = TRUE)}}"
        ),
        format_done = paste0(
          "{cli::pb_name} | Done in {cli::pb_elapsed} | ",
          "Final LL: {.val {format(ll, digits = 4, nsmall = 2)}}"
        ),
        .envir = parent.frame()
      )
    } else if (.has_cli()) {
      cli::cli_alert_info("Starting {solver_name}...")
    } else {
      message(sprintf("Starting %s...", solver_name))
    }
  }

  update <- function(iter, loglike, grad_norm = NULL) {
    if (iter %% show_every != 0) return(invisible(NULL))

    improvement <- loglike - last_ll
    last_ll <<- loglike

    if (.has_cli() && !is.null(max_iter)) {
      # Update progress bar
      ll <- loglike
      cli::cli_progress_update(set = iter)
    } else if (.has_cli()) {
      # Inline status
      grad_str <- if (!is.null(grad_norm)) {
        sprintf(" |grad|=%.2e", grad_norm)
      } else ""
      cli::cli_status(
        "{solver_name} iter {iter}: LL={format(loglike, digits=4)}{grad_str}"
      )
    } else {
      # Base R message
      grad_str <- if (!is.null(grad_norm)) {
        sprintf(" |grad|=%.2e", grad_norm)
      } else ""
      if (iter %% 10 == 0 || iter == 1) {
        message(sprintf("%s iter %d: LL=%.4f%s",
                       solver_name, iter, loglike, grad_str))
      }
    }
  }

  finish <- function(converged, iterations, final_ll) {
    elapsed <- if (!is.null(start_time)) {
      difftime(Sys.time(), start_time, units = "secs")
    } else NA

    if (.has_cli() && !is.null(max_iter)) {
      ll <- final_ll
      cli::cli_progress_done()
    }

    status <- if (converged) "converged" else "max iterations"

    if (.has_cli()) {
      if (converged) {
        cli::cli_alert_success(
          "{solver_name} {status} in {iterations} iterations (LL={format(final_ll, digits=4)})"
        )
      } else {
        cli::cli_alert_warning(
          "{solver_name} stopped at {status} ({iterations} iterations, LL={format(final_ll, digits=4)})"
        )
      }
    } else {
      message(sprintf("%s %s in %d iterations (LL=%.4f)",
                     solver_name, status, iterations, final_ll))
    }
  }

  list(start = start, update = update, finish = finish)
}
