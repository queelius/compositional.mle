# Newton-Raphson Solver

Creates a solver that uses Newton-Raphson (second-order) optimization.
Uses the Fisher information matrix to scale the gradient for faster
convergence near the optimum.

## Usage

``` r
newton_raphson(
  line_search = TRUE,
  max_iter = 50L,
  tol = 1e-08,
  backtrack_ratio = 0.5,
  min_step = 1e-12,
  verbose = FALSE
)
```

## Arguments

- line_search:

  Use backtracking line search for stability

- max_iter:

  Maximum number of iterations

- tol:

  Convergence tolerance (on parameter change)

- backtrack_ratio:

  Step size reduction factor for line search

- min_step:

  Minimum step size before giving up

- verbose:

  Logical; if TRUE and the cli package is installed, display progress
  during optimization. Default is FALSE.

## Value

A solver function with signature (problem, theta0, trace) -\> mle_result

## Details

Newton-Raphson computes the search direction as \\I(\theta)^{-1}
s(\theta)\\ where \\I\\ is the Fisher information and \\s\\ is the
score. This accounts for parameter scaling and typically converges
faster than gradient ascent when near the optimum.

Requires the problem to have a Fisher information function (either
analytic or computed numerically).

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage
solver <- newton_raphson()
result <- solver(problem, c(0, 1))

# Often used after gradient ascent for refinement
strategy <- gradient_ascent(max_iter = 50) %>>% newton_raphson(max_iter = 20)
} # }
```
