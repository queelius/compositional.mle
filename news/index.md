# Changelog

## compositional.mle 1.0.0

### CRAN Release

- First CRAN release
- Moved `algebraic.mle` from Imports to Depends so that generics
  ([`params()`](https://queelius.github.io/algebraic.dist/reference/params.html),
  [`se()`](https://queelius.github.io/algebraic.mle/reference/se.html),
  [`confint()`](https://rdrr.io/r/stats/confint.html),
  [`loglik_val()`](https://queelius.github.io/algebraic.mle/reference/loglik_val.html),
  [`aic()`](https://queelius.github.io/algebraic.mle/reference/aic.html),
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
- Convergence diagnostics:
  [`plot.mle_numerical()`](https://queelius.github.io/compositional.mle/reference/plot.mle_numerical.md)
  and
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
