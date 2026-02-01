# Fisher Scoring Solver

Variant of Newton-Raphson that uses the expected Fisher information
instead of the observed Fisher. Can be more stable for some problems.

## Usage

``` r
fisher_scoring(
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

A solver function

## Details

Fisher scoring is identical to Newton-Raphson when the expected and
observed Fisher information are equal (e.g., exponential families). For
other models, it may have different convergence properties.
