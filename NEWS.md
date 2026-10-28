# compositional.mle 2.0.1

* Failed solvers retain their result slots in `race()` and `with_restarts()`.
  Successful restarts are now returned even when later attempts fail, and
  race alternatives preserve the original solver ordering. All-failure runs
  report the intended error instead of an out-of-bounds subscript.

# compositional.mle 2.0.0

## Breaking Changes

* All solvers now return `solver_result` objects (class vector:
  `c("solver_result", "mle_fit_numerical", "mle_fit")`). Per-solver subclasses
  (`mle_gradient_ascent`, `mle_bfgs`, etc.) are removed.
* Solver metadata moved to `$solver_info` nested list. Access solver name via
  `result$solver_info$name`, iterations via `result$solver_info$iterations`,
  trace data via `result$solver_info$trace_data`.
* Composition metadata moved to `$solver_info$composition`. Access strategy via
  `result$solver_info$composition$strategy`, chain results via
  `result$solver_info$composition$chain`, etc.
* Solver-specific diagnostics moved to `$solver_info$diagnostics` (e.g.,
  `$diagnostics$final_temp` for sim_anneal, `$diagnostics$cycles` for
  coordinate_ascent).
* `is_mle_numerical()` replaced by `is_solver_result()`.
* `compose()` removed from exports — use `chain()` instead (superset with `early_stop`).
* `mle_problem()` is now an S3 generic dispatching on its first argument.
* Requires `algebraic.mle >= 2.0.0`.

## New Features

* `mle_problem.likelihood_model()`: Bridge to the `likelihood.model` package.
  Pass a `likelihood_model` object and data to auto-extract loglik/score/fisher.
* `fisher_scoring()` now correctly reports `"fisher_scoring"` as solver name
  (previously reported `"newton_raphson"`).
* `coordinate_ascent()` gains `learning_rate` parameter for non-line-search mode.
* `grid_search()` now errors if the grid would exceed 1 million points.
* `race()` warns when `parallel = TRUE` but `future` is not installed.

## Bug Fixes

* `sim_anneal()` now correctly reports convergence based on temperature cooling
  (previously always reported `converged = TRUE`).
* `coordinate_ascent()` non-line-search mode uses scaled gradient step instead
  of hardcoded 0.1 step.
* `print.mle_trace_data()` no longer accesses vector elements before null-check.
* `print.mle_problem()` correctly detects default (unconstrained) problems.

# compositional.mle 1.0.0

## CRAN Release

- First CRAN release
- Moved `algebraic.mle` from Imports to Depends so that generics
  (`params()`, `se()`, `confint()`, `loglik_val()`, `aic()`, `nparams()`,
  `vcov()`) are available immediately when the package is loaded
- Converted `\dontrun{}` to `\donttest{}` in examples (CRAN policy)
- Added `hypothesize` to Suggests
- Added CI via GitHub Actions

# compositional.mle 0.2.0

## New Features
- Simulated annealing solver (`sim_anneal()`)
- Coordinate ascent solver (`coordinate_ascent()`)
- `race()` function for explicit parallel solver racing with `future` support
- `chain()` function for sequential composition with early stopping
- Convergence diagnostics: `plot.mle_numerical()` and `optimization_path()`
- Derivative caching in `mle_problem()` to avoid redundant computation
- Verbose/progress output via `cli` for long-running solvers
- Trace aggregation with `merge_traces()` across composed solvers

## Documentation
- Four vignettes: getting-started, theory-and-intuition, case-studies, strategy-design
- pkgdown site at https://queelius.github.io/compositional.mle/

## Bug Fixes
- Fixed R CMD check issues (missing imports, example errors)
- Fixed LICENSE copyright holder

# compositional.mle 0.1.0

- Initial release with core solver factories and composition operators
- Solvers: gradient_ascent, newton_raphson, bfgs, lbfgsb, nelder_mead, grid_search, random_search
- Composition: %>>% (sequential), %|% (racing), with_restarts(), unless_converged()
- Problem specification with mle_problem() and constraint support
- Tracing system for optimization iteration recording
- Penalty/regularization transformers (L1, L2, elastic net)
