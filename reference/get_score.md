# Get score function from problem

Returns the score (gradient) function, computing numerically if not
provided. If `cache_derivatives = TRUE` was set in the problem and score
is computed numerically, results are cached using a single-value cache.

## Usage

``` r
get_score(problem)
```

## Arguments

- problem:

  An mle_problem object

## Value

Score function that takes a parameter vector and returns the gradient of
the log-likelihood.

## Examples

``` r
problem <- mle_problem(
  loglike = function(theta) -sum((theta - c(1, 2))^2)
)
score_fn <- get_score(problem)
score_fn(c(0, 0))  # Gradient at (0, 0)
#> [1] 2 4
```
