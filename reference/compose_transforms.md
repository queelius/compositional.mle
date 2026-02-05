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
#> Error in compose_transforms(function(f) with_penalty(f, penalty_l1(),     lambda = 0.01), function(f) with_penalty(f, penalty_l2(),     lambda = 0.05)): could not find function "compose_transforms"

# Apply to log-likelihood
loglike <- function(theta) -sum((theta - c(1, 2))^2)
loglike_transformed <- transform(loglike)
#> Error in as.data.frame.default(x[[i]], optional = TRUE): cannot coerce class ‘"function"’ to a data.frame
loglike_transformed(c(1, 2))
#> Error in loglike_transformed(c(1, 2)): could not find function "loglike_transformed"
# }
```
