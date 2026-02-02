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
if (FALSE) { # \dontrun{
# Create a composition
transform <- compose_transforms(
  function(f) with_penalty(f, penalty_l1(), lambda = 0.01),
  function(f) with_subsampling(f, data, 50)
)

# Apply to log-likelihood
loglike_transformed <- transform(loglike)

# Equivalent to:
loglike_transformed <- loglike %>%
  with_subsampling(data, 50) %>%
  with_penalty(penalty_l1(), lambda = 0.01)
} # }
```
