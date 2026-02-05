# Clear derivative cache

Clears the cached numerical derivatives (score and Fisher) from an
mle_problem. This is useful when you want to force recomputation, for
example after modifying data that the log-likelihood depends on.

## Usage

``` r
clear_cache(problem)
```

## Arguments

- problem:

  An mle_problem object

## Value

The problem object (invisibly), modified in place

## Examples

``` r
# \donttest{
loglike <- function(theta) -sum((theta - c(1, 2))^2)
problem <- mle_problem(loglike, cache_derivatives = TRUE)
#> Error in mle_problem(loglike, cache_derivatives = TRUE): unused argument (cache_derivatives = TRUE)
# ... run some optimization ...
clear_cache(problem)  # Force fresh derivative computation
#> Error in clear_cache(problem): could not find function "clear_cache"
# }
```
