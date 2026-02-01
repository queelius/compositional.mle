# Compose Multiple Solvers Sequentially

Chains any number of solvers sequentially. Each solver's result becomes
the starting point for the next. Alternative to using `%>>%` operator.

## Usage

``` r
compose(...)
```

## Arguments

- ...:

  Solver functions to compose

## Value

A new solver function that runs all solvers in sequence

## Details

Trace data from all solvers is merged into a single trace with stage
boundaries preserved.

## Examples

``` r
if (FALSE) { # \dontrun{
# Three-stage strategy
strategy <- compose(
  grid_search(n = 5),
  gradient_ascent(max_iter = 50),
  newton_raphson(max_iter = 20)
)
result <- strategy(problem, theta0)
} # }
```
