# Race Multiple Solvers

Runs multiple solvers (optionally in parallel) and returns the best
result (highest log-likelihood). More flexible than `%|%` operator.

## Usage

``` r
race(..., parallel = FALSE)
```

## Arguments

- ...:

  Solver functions to race

- parallel:

  Logical; if TRUE and the future package is installed, solvers are run
  in parallel using the current future plan. Default is FALSE.

## Value

A new solver function that races all solvers and picks the best

## Details

When `parallel = TRUE`, solvers are executed using `future::future()`
and results collected with `future::value()`. The current future plan
determines how parallelization happens (e.g., `plan(multisession)` for
multi-process execution).

Failed solvers (those that throw errors) are ignored. If all solvers
fail, an error is thrown.

## Examples

``` r
# Race three methods sequentially
strategy <- race(gradient_ascent(), bfgs(), nelder_mead())
#> Error in race(gradient_ascent(), bfgs(), nelder_mead()): could not find function "race"

# Race with parallel execution (requires future package)
if (FALSE) { # \dontrun{
future::plan(future::multisession)
strategy <- race(gradient_ascent(), bfgs(), nelder_mead(), parallel = TRUE)
} # }
```
