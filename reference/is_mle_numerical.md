# Check if object is an mle_numerical

Check if object is an mle_numerical

## Usage

``` r
is_mle_numerical(x)
```

## Arguments

- x:

  Object to test

## Value

Logical indicating whether `x` inherits from `mle_numerical`.

## Examples

``` r
# \donttest{
problem <- mle_problem(
  loglike = function(theta) -sum((theta - c(1, 2))^2)
)
result <- gradient_ascent(max_iter = 20)(problem, c(0, 0))
is_mle_numerical(result)  # TRUE
#> [1] TRUE
is_mle_numerical(list())  # FALSE
#> [1] FALSE
# }
```
