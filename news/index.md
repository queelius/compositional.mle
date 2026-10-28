# Changelog

## compositional.mle 2.0.1

- Failed solvers retain their result slots in
  [`race()`](https://queelius.github.io/compositional.mle/reference/race.md)
  and
  [`with_restarts()`](https://queelius.github.io/compositional.mle/reference/with_restarts.md).
  Successful restarts are now returned even when later attempts fail,
  and race alternatives preserve the original solver ordering.
  All-failure runs report the intended error instead of an out-of-bounds
  subscript.

## compositional.mle 2.0.0

CRAN release: 2026-03-19

### Breaking Changes

- All solvers now return `solver_result` objects (class vector:
  `c("solver_result", "mle_fit_numerical", "mle_fit")`). Per-solver
  subclasses (`mle_gradient_ascent`, `mle_bfgs`, etc.) are removed.
- Solver metadata moved to `$solver_info` nested list. Access solver
  name via `result$solver_info$name`, iterations via
  `result$solver_info$iterations`, trace data via
  `result$solver_info$trace_data`.
- Composition metadata moved to `$solver_info$composition`. Access
  strategy via `result$solver_info$composition$strategy`, chain results
  via `result$solver_info$composition$chain`, etc.
- Solver-specific diagnostics moved to `$solver_info$diagnostics` (e.g.,
  `$diagnostics$final_temp` for sim_anneal, `$diagnostics$cycles` for
  coordinate_ascent).
- `is_mle_numerical()` replaced by
  [`is_solver_result()`](https://queelius.github.io/compositional.mle/reference/is_solver_result.md).
- [`compose()`](https://queelius.github.io/compositional.mle/reference/compose.md)
  removed from exports — use
  [`chain()`](https://queelius.github.io/compositional.mle/reference/chain.md)
  instead (superset with `early_stop`).
- [`mle_problem()`](https://queelius.github.io/compositional.mle/reference/mle_problem.md)
  is now an S3 generic dispatching on its first argument.
- Requires `algebraic.mle >= 2.0.0`.

### New Features

- [`mle_problem.likelihood_model()`](https://queelius.github.io/compositional.mle/reference/mle_problem.md):
  Bridge to the `likelihood.model` package. Pass a `likelihood_model`
  object and data to auto-extract loglik/score/fisher.
- [`fisher_scoring()`](https://queelius.github.io/compositional.mle/reference/fisher_scoring.md)
  now correctly reports `"fisher_scoring"` as solver name (previously
  reported `"newton_raphson"`).
- [`coordinate_ascent()`](https://queelius.github.io/compositional.mle/reference/coordinate_ascent.md)
  gains `learning_rate` parameter for non-line-search mode.
- [`grid_search()`](https://queelius.github.io/compositional.mle/reference/grid_search.md)
  now errors if the grid would exceed 1 million points.
- [`race()`](https://queelius.github.io/compositional.mle/reference/race.md)
  warns when `parallel = TRUE` but `future` is not installed.

### Bug Fixes

- [`sim_anneal()`](https://queelius.github.io/compositional.mle/reference/sim_anneal.md)
  now correctly reports convergence based on temperature cooling
  (previously always reported `converged = TRUE`).
- [`coordinate_ascent()`](https://queelius.github.io/compositional.mle/reference/coordinate_ascent.md)
  non-line-search mode uses scaled gradient step instead of hardcoded
  0.1 step.
- [`print.mle_trace_data()`](https://queelius.github.io/compositional.mle/reference/print.mle_trace_data.md)
  no longer accesses vector elements before null-check.
- [`print.mle_problem()`](https://queelius.github.io/compositional.mle/reference/mle_problem.md)
  correctly detects default (unconstrained) problems.

## compositional.mle 1.0.0

### CRAN Release

- First CRAN release
- Moved `algebraic.mle` from Imports to Depends so that generics
  ([`params()`](https://queelius.github.io/algebraic.dist/reference/params.html),
  [`se()`](https://queelius.github.io/algebraic.mle/reference/se.html),
  [`confint()`](https://rdrr.io/r/stats/confint.html), `loglik_val()`,
  `aic()`,
  [`nparams()`](https://queelius.github.io/algebraic.dist/reference/nparams.html),
  [`vcov()`](https://rdrr.io/r/stats/vcov.html)) are available
  immediately when the package is loaded
- Converted `\dontrun{}` to `\donttest{}` in examples (CRAN policy)
- Added `hypothesize` to Suggests
- Added CI via GitHub Actions

## compositional.mle 0.2.0

### New Features

- Simulated annealing solver
  ([`sim_anneal()`](https://queelius.github.io/compositional.mle/reference/sim_anneal.md))
- Coordinate ascent solver
  ([`coordinate_ascent()`](https://queelius.github.io/compositional.mle/reference/coordinate_ascent.md))
- [`race()`](https://queelius.github.io/compositional.mle/reference/race.md)
  function for explicit parallel solver racing with `future` support
- [`chain()`](https://queelius.github.io/compositional.mle/reference/chain.md)
  function for sequential composition with early stopping
- Convergence diagnostics: `plot.mle_numerical()` and
  [`optimization_path()`](https://queelius.github.io/compositional.mle/reference/optimization_path.md)
- Derivative caching in
  [`mle_problem()`](https://queelius.github.io/compositional.mle/reference/mle_problem.md)
  to avoid redundant computation
- Verbose/progress output via `cli` for long-running solvers
- Trace aggregation with
  [`merge_traces()`](https://queelius.github.io/compositional.mle/reference/merge_traces.md)
  across composed solvers

### Documentation

- Four vignettes: getting-started, theory-and-intuition, case-studies,
  strategy-design
- pkgdown site at <https://queelius.github.io/compositional.mle/>

### Bug Fixes

- Fixed R CMD check issues (missing imports, example errors)
- Fixed LICENSE copyright holder

## compositional.mle 0.1.0

- Initial release with core solver factories and composition operators
- Solvers: gradient_ascent, newton_raphson, bfgs, lbfgsb, nelder_mead,
  grid_search, random_search
- Composition: %\>\>% (sequential), %\|% (racing), with_restarts(),
  unless_converged()
- Problem specification with mle_problem() and constraint support
- Tracing system for optimization iteration recording
- Penalty/regularization transformers (L1, L2, elastic net)
