# Check if object is a solver result

Check if object is a solver result

## Usage

``` r
is_solver_result(x)
```

## Arguments

- x:

  Object to test

## Value

Logical indicating whether `x` inherits from `solver_result`.

## Examples

``` r
# \donttest{
problem <- mle_problem(
  loglike = function(theta) -sum((theta - c(1, 2))^2)
)
result <- gradient_ascent(max_iter = 20)(problem, c(0, 0))
is_solver_result(result)  # TRUE
#> [1] TRUE
is_solver_result(list())  # FALSE
#> [1] FALSE
# }
```
