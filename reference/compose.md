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
# \donttest{
set.seed(42)
x <- rnorm(50, 5, 2)
problem <- mle_problem(
  loglike = function(theta) sum(dnorm(x, theta[1], theta[2], log = TRUE)),
  constraint = mle_constraint(support = function(theta) theta[2] > 0,
                              project = function(theta) c(theta[1], max(theta[2], 1e-8)))
)
# Three-stage strategy
strategy <- compose(
  grid_search(lower = c(-10, 0.1), upper = c(10, 5), n = 5),
  gradient_ascent(max_iter = 50),
  newton_raphson(max_iter = 20)
)
result <- strategy(problem, c(0, 1))
# }
```
