# Check if solver converged

Check if solver converged

## Usage

``` r
is_converged(x, ...)
```

## Arguments

- x:

  An mle result object

- ...:

  Additional arguments (unused)

## Value

Logical indicating convergence

## Examples

``` r
# \donttest{
problem <- mle_problem(
  loglike = function(theta) -sum((theta - c(1, 2))^2)
)
result <- gradient_ascent(max_iter = 50)(problem, c(0, 0))
is_converged(result)
#> [1] TRUE
# }
```
