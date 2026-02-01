# Create an MLE Problem Specification

Encapsulates a maximum likelihood estimation problem, separating the
statistical specification from the optimization strategy.

## Usage

``` r
mle_problem(
  loglike,
  score = NULL,
  fisher = NULL,
  constraint = NULL,
  theta_names = NULL,
  n_obs = NULL,
  cache_derivatives = FALSE
)
```

## Arguments

- loglike:

  Log-likelihood function taking parameter vector theta

- score:

  Score function (gradient of log-likelihood). If NULL, computed
  numerically via numDeriv::grad when needed.

- fisher:

  Fisher information matrix function. If NULL, computed numerically via
  numDeriv::hessian when needed.

- constraint:

  Domain constraints as mle_constraint object

- theta_names:

  Character vector of parameter names for nice output

- n_obs:

  Number of observations (for AIC/BIC computation)

- cache_derivatives:

  Logical; if TRUE and score/fisher are computed numerically, cache the
  most recent result to avoid redundant computation. This is
  particularly useful during line search where the same point may be
  evaluated multiple times. Default is FALSE.

## Value

An mle_problem object

## Details

The problem object provides lazy evaluation of derivatives. If you don't
provide analytic score or fisher functions, they will be computed
numerically when requested.

When `cache_derivatives = TRUE`, numerical derivatives are cached using
a single-value cache (stores the most recent theta and result). This is
efficient for optimization where consecutive calls often evaluate at the
same point (e.g., during line search or convergence checking). Use
[`clear_cache`](https://queelius.github.io/compositional.mle/reference/clear_cache.md)
to manually clear the cache if needed.

## Examples

``` r
# With analytic derivatives
problem <- mle_problem(
  loglike = function(theta) sum(dnorm(data, theta[1], theta[2], log = TRUE)),
  score = function(theta) {
    c(sum(data - theta[1]) / theta[2]^2,
      -length(data)/theta[2] + sum((data - theta[1])^2) / theta[2]^3)
  },
  constraint = mle_constraint(
    support = function(theta) theta[2] > 0,
    project = function(theta) c(theta[1], max(theta[2], 1e-8))
  ),
  theta_names = c("mu", "sigma")
)

# Without analytic derivatives (computed numerically)
problem <- mle_problem(
  loglike = function(theta) sum(dnorm(data, theta[1], theta[2], log = TRUE)),
  constraint = mle_constraint(
    support = function(theta) theta[2] > 0
  )
)
```
