# Compose Multiple Function Transformations

Applies transformations right-to-left (like mathematical composition).
This allows building complex log-likelihood transformations from simple
ones.

## Usage

``` r
compose_transforms(...)
```

## Arguments

- ...:

  Transformer functions

## Value

Composed transformer function

## Details

Note: For composing solvers, use
[`compose`](https://queelius.github.io/compositional.mle/reference/compose.md)
instead.

## Examples

``` r
# \donttest{
# Create a composition of transformations
transform <- compose_transforms(
  function(f) with_penalty(f, penalty_l1(), lambda = 0.01),
  function(f) with_penalty(f, penalty_l2(), lambda = 0.05)
)

# Apply to log-likelihood
loglike <- function(theta) -sum((theta - c(1, 2))^2)
loglike_transformed <- transform(loglike)
loglike_transformed(c(1, 2))
#> [1] -0.28
# }
```
