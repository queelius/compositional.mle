# BFGS Solver

Creates a solver using the BFGS quasi-Newton method via
[`optim()`](https://rdrr.io/r/stats/optim.html). BFGS approximates the
Hessian from gradient information, providing second-order-like
convergence without computing the Hessian directly.

## Usage

``` r
bfgs(max_iter = 100L, tol = 1e-08, report = 0L)
```

## Arguments

- max_iter:

  Maximum number of iterations

- tol:

  Convergence tolerance (passed to optim's reltol)

- report:

  Reporting frequency (0 = no reporting)

## Value

A solver function with signature (problem, theta0, trace) -\> mle_result

## Details

BFGS is often a good default choice: it's more robust than
Newton-Raphson (no matrix inversion issues) and faster than gradient
ascent (uses curvature information).

The solver automatically uses the score function from the problem if
available, otherwise computes gradients numerically.

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
# Basic usage
result <- bfgs()(problem, c(4, 1.5))

# Race BFGS against gradient ascent
strategy <- bfgs() %|% gradient_ascent()
# }
```
