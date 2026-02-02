# L-BFGS-B Solver (Box Constrained)

Creates a solver using L-BFGS-B, a limited-memory BFGS variant that
supports box constraints (lower and upper bounds on parameters).

## Usage

``` r
lbfgsb(lower = -Inf, upper = Inf, max_iter = 100L, tol = 1e-08)
```

## Arguments

- lower:

  Lower bounds on parameters (can be -Inf)

- upper:

  Upper bounds on parameters (can be Inf)

- max_iter:

  Maximum number of iterations

- tol:

  Convergence tolerance

## Value

A solver function

## Details

Unlike the constraint system in mle_problem (which uses projection),
L-BFGS-B handles box constraints natively within the algorithm. Use this
when you have simple bound constraints.

## Examples

``` r
# \donttest{
set.seed(42)
x <- rnorm(50, 5, 2)
problem <- mle_problem(
  loglike = function(theta) sum(dnorm(x, theta[1], theta[2], log = TRUE))
)
# Positive sigma via box constraint
solver <- lbfgsb(lower = c(-Inf, 0.01), upper = c(Inf, Inf))
result <- solver(problem, c(4, 1.5))
# }
```
