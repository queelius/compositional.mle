# Chain Solvers with Early Stopping

Chains multiple solvers sequentially with optional early stopping. More
flexible than `%>>%` operator.

## Usage

``` r
chain(..., early_stop = NULL)
```

## Arguments

- ...:

  Solver functions to chain

- early_stop:

  Optional function that takes a result and returns TRUE to stop the
  chain early. Default is NULL (no early stopping).

## Value

A new solver function that runs solvers in sequence

## Details

The chain runs solvers in order, passing each result's `theta.hat` to
the next solver. If `early_stop` is provided and returns TRUE for any
intermediate result, the chain stops early.

Common early stopping conditions:

- Stop when converged: `function(r) r$converged`

- Stop when gradient is small: `function(r) sqrt(sum(score^2)) < 1e-6`

- Stop after reaching target: `function(r) r$loglike > -100`

## Examples

``` r
# Chain with early stopping when converged
strategy <- chain(
  grid_search(lower = c(-10, 0.1), upper = c(10, 5), n = 5),
  gradient_ascent(max_iter = 50),
  newton_raphson(max_iter = 20),
  early_stop = function(r) isTRUE(r$converged)
)
#> Error in chain(grid_search(lower = c(-10, 0.1), upper = c(10, 5), n = 5),     gradient_ascent(max_iter = 50), newton_raphson(max_iter = 20),     early_stop = function(r) isTRUE(r$converged)): could not find function "chain"

# Standard chain (no early stopping)
strategy <- chain(gradient_ascent(), newton_raphson())
#> Error in chain(gradient_ascent(), newton_raphson()): could not find function "chain"
```
