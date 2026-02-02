# Create stochastic log-likelihood with subsampling

Transforms a log-likelihood function to use only a random subsample of
observations. Useful for stochastic gradient ascent on large datasets.

## Usage

``` r
with_subsampling(loglike, data, subsample_size, replace = FALSE)
```

## Arguments

- loglike:

  Base log-likelihood function. Should accept theta and data.

- data:

  Observations (vector, matrix, or data.frame)

- subsample_size:

  Number of observations to sample per evaluation

- replace:

  Sample with replacement (logical, default: FALSE)

## Value

Transformed log-likelihood function

## Examples

``` r
# \donttest{
# Original likelihood uses all data
data <- rnorm(10000, mean = 5, sd = 2)

loglike <- function(theta, obs = data) {
  sum(dnorm(obs, mean = theta[1], sd = theta[2], log = TRUE))
}

# Stochastic version uses random subsample
loglike_stoch <- with_subsampling(
  loglike,
  data = data,
  subsample_size = 100
)

# Each call uses different random subsample
loglike_stoch(c(5, 2))
#> [1] -207.7651
loglike_stoch(c(5, 2))  # Different value
#> [1] -216.0809
# }
```
