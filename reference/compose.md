# Compose Multiple Solvers Sequentially

Chains any number of solvers sequentially. Each solver's result becomes
the starting point for the next. Alternative to using `%>>%` operator.

Applies transformations right-to-left (like mathematical composition).
This allows building complex transformations from simple ones.

## Usage

``` r
compose(...)

compose(...)
```

## Arguments

- ...:

  Transformer functions

## Value

A new solver function that runs all solvers in sequence

Composed transformer function

## Examples

``` r
if (FALSE) { # \dontrun{
# Three-stage strategy
strategy <- compose(
  grid_search(n = 5),
  gradient_ascent(max_iter = 50),
  newton_raphson(max_iter = 20)
)
result <- strategy(problem, theta0)
} # }

if (FALSE) { # \dontrun{
# Create a composition
transform <- compose(
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
