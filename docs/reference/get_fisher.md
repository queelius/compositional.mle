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

Fisher information function
