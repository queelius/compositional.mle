# Check if object is an mle_problem

Check if object is an mle_problem

## Usage

``` r
is_mle_problem(x)
```

## Arguments

- x:

  Object to test

## Value

Logical indicating whether `x` is an `mle_problem`.

## Examples

``` r
problem <- mle_problem(
  loglike = function(theta) -sum((theta - c(1, 2))^2)
)
is_mle_problem(problem)  # TRUE
#> [1] TRUE
is_mle_problem(list())   # FALSE
#> [1] FALSE
```
