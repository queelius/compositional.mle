# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Package Overview

`compositional.mle` is an R package for composable maximum likelihood
estimation. Solvers are first-class functions that combine via
operators: sequential chaining (`%>>%`), parallel racing (`%|%`), and
random restarts (`with_restarts`). The design follows SICP principles
where combining solvers yields a solver (closure property).

Key dependencies: `algebraic.mle` (\>= 2.0.0) provides the base `mle`
class for results. `likelihood.model` (optional, in Suggests) enables
direct bridge via `mle_problem(model, data)`.

## Development Commands

``` bash
# Load for development
devtools::load_all()

# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-solvers.R")

# Generate documentation (roxygen2)
devtools::document()

# Full package check
devtools::check()

# Test coverage analysis
covr::package_coverage()

# Build pkgdown site
pkgdown::build_site()
```

## Architecture

### Core Design Pattern

Solvers are **factory functions** that return solver functions with
uniform signature:

``` r
(problem, theta0, trace) -> mle_result
```

This enables composition:

``` r

# Coarse-to-fine: grid -> gradient -> Newton
strategy <- grid_search(n = 5) %>>% gradient_ascent() %>>% newton_raphson()

# Race different methods, pick best
strategy <- gradient_ascent() %|% bfgs() %|% nelder_mead()

# Multiple random restarts
strategy <- with_restarts(gradient_ascent(), n = 20, sampler = uniform_sampler(lower, upper))

# Sequential composition with early stopping (replaces removed compose())
strategy <- chain(gradient_ascent(), newton_raphson(), early_stop = TRUE)
```

### Key Abstractions

| File | Purpose |
|----|----|
| `R/problem.R` | [`mle_problem()`](https://queelius.github.io/compositional.mle/reference/mle_problem.md) - S3 generic; dispatches on first arg; [`mle_problem.likelihood_model()`](https://queelius.github.io/compositional.mle/reference/mle_problem.md) bridges to `likelihood.model` package |
| `R/compose.R` | Composition operators: `%>>%`, `%|%`, [`chain()`](https://queelius.github.io/compositional.mle/reference/chain.md), [`race()`](https://queelius.github.io/compositional.mle/reference/race.md), [`with_restarts()`](https://queelius.github.io/compositional.mle/reference/with_restarts.md), [`unless_converged()`](https://queelius.github.io/compositional.mle/reference/unless_converged.md) |
| `R/trace.R` | [`mle_trace()`](https://queelius.github.io/compositional.mle/reference/mle_trace.md) - configurable iteration tracing |
| `R/config.R` | [`mle_constraint()`](https://queelius.github.io/compositional.mle/reference/mle_constraint.md) - type-safe constraint objects |

### Solver Factories

| Factory | Description | Method |
|----|----|----|
| [`gradient_ascent()`](https://queelius.github.io/compositional.mle/reference/gradient_ascent.md) | Steepest ascent with optional line search | Native |
| [`newton_raphson()`](https://queelius.github.io/compositional.mle/reference/newton_raphson.md) | Second-order using Fisher information | Native |
| [`bfgs()`](https://queelius.github.io/compositional.mle/reference/bfgs.md) | Quasi-Newton BFGS | [`optim()`](https://rdrr.io/r/stats/optim.html) wrapper |
| [`lbfgsb()`](https://queelius.github.io/compositional.mle/reference/lbfgsb.md) | L-BFGS-B with box constraints | [`optim()`](https://rdrr.io/r/stats/optim.html) wrapper |
| [`nelder_mead()`](https://queelius.github.io/compositional.mle/reference/nelder_mead.md) | Derivative-free simplex | [`optim()`](https://rdrr.io/r/stats/optim.html) wrapper |
| [`grid_search()`](https://queelius.github.io/compositional.mle/reference/grid_search.md) | Exhaustive grid evaluation | Native |
| [`random_search()`](https://queelius.github.io/compositional.mle/reference/random_search.md) | Random sampling | Native |

### Result Objects

All solvers return `solver_result` objects (class vector:
`c("solver_result", "mle_fit_numerical", "mle_fit")`) with: -
`$theta.hat` - MLE estimate - `$loglike` - log-likelihood at MLE -
`$converged` - convergence flag - `$solver_info` - nested list
containing all solver metadata: - `$solver_info$name` - solver name
(e.g., `"gradient_ascent"`, `"bfgs"`) - `$solver_info$iterations` -
iteration count - `$solver_info$trace_data` - optimization trace (if
tracing enabled) - `$solver_info$diagnostics` - solver-specific
diagnostics (e.g., `$final_temp` for `sim_anneal`) -
`$solver_info$composition` - composition metadata (e.g., `$strategy`,
`$chain`, `$best_restart`, `$n_restarts`)

Use
[`is_solver_result()`](https://queelius.github.io/compositional.mle/reference/is_solver_result.md)
to test for this class (replaces old `is_mle_numerical()`).

## Typical Workflow

``` r

# 1. Define the problem
problem <- mle_problem(
  loglike = function(theta) sum(dnorm(data, theta[1], theta[2], log = TRUE)),
  score = function(theta) {...},  # Optional, computed numerically if NULL
  constraint = mle_constraint(
    support = function(theta) theta[2] > 0,
    project = function(theta) c(theta[1], max(theta[2], 1e-8))
  )
)

# 2. Create solver strategy
solver <- gradient_ascent() %>>% newton_raphson()

# 3. Solve
result <- solver(problem, theta0 = c(0, 1))
```

## Testing Notes

- Tests are in `tests/testthat/` with test files for each major
  component
- Standard normal MLE is used as the canonical test case
- Solvers are tested for convergence to known true values with tolerance
- Tests depend on `algebraic.mle` being installed
