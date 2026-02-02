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
