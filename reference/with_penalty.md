# Add penalty term to log-likelihood

Transforms a log-likelihood by subtracting a penalty term. Useful for
regularized estimation (e.g., LASSO, Ridge regression).

## Usage

``` r
with_penalty(loglike, penalty, lambda = 1)
```

## Arguments

- loglike:

  Base log-likelihood function

- penalty:

  Penalty function taking theta and returning numeric

- lambda:

  Penalty weight (non-negative numeric, default: 1.0)

## Value

Transformed log-likelihood function

## Examples

``` r
# \donttest{
# Regression with L2 penalty (Ridge)
loglike <- function(theta) -sum((theta - c(1, 2))^2)

# Add L2 penalty
loglike_penalized <- with_penalty(
  loglike,
  penalty = penalty_l2(),
  lambda = 0.1
)
loglike_penalized(c(1, 2))  # Evaluate penalized likelihood
#> [1] -0.5
# }
```
