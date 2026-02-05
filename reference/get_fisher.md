# Get Fisher information function from problem

Returns the Fisher information matrix function, computing numerically if
not provided. If `cache_derivatives = TRUE` was set in the problem and
Fisher is computed numerically, results are cached using a single-value
cache.

## Usage

``` r
get_fisher(problem)
```

## Arguments

- problem:

  An mle_problem object

## Value

Fisher information function that takes a parameter vector and returns
the Fisher information matrix (negative Hessian of log-likelihood).

## Examples

``` r
problem <- mle_problem(
  loglike = function(theta) -sum((theta - c(1, 2))^2)
)
fisher_fn <- get_fisher(problem)
fisher_fn(c(1, 2))  # Fisher information at the optimum
#>              [,1]         [,2]
#> [1,]  2.00000e+00 -1.52056e-17
#> [2,] -1.52056e-17  2.00000e+00
```
