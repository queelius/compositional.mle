# Get number of iterations

Get number of iterations

## Usage

``` r
num_iterations(x, ...)
```

## Arguments

- x:

  An mle result object

- ...:

  Additional arguments (unused)

## Value

Number of iterations, or `NA_integer_` if not available.

## Examples

``` r
# \donttest{
problem <- mle_problem(
  loglike = function(theta) -sum((theta - c(1, 2))^2)
)
result <- gradient_ascent(max_iter = 50)(problem, c(0, 0))
num_iterations(result)
#> [1] 14
# }
```
