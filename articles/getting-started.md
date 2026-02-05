# Getting Started with compositional.mle

## Introduction

The `compositional.mle` package provides **composable** optimization
strategies for maximum likelihood estimation (MLE). Following SICP
principles, it offers:

1.  **Primitive solvers** -
    [`gradient_ascent()`](https://queelius.github.io/compositional.mle/reference/gradient_ascent.md),
    [`newton_raphson()`](https://queelius.github.io/compositional.mle/reference/newton_raphson.md),
    [`bfgs()`](https://queelius.github.io/compositional.mle/reference/bfgs.md),
    [`nelder_mead()`](https://queelius.github.io/compositional.mle/reference/nelder_mead.md),
    etc.
2.  **Composition operators** - `%>>%` (sequential), `%|%` (race),
    [`with_restarts()`](https://queelius.github.io/compositional.mle/reference/with_restarts.md)
3.  **Closure property** - Combining solvers yields a solver

## Installation

``` r
devtools::install_github("queelius/compositional.mle")
```

``` r
library(compositional.mle)
```

## Quick Start: Normal Distribution MLE

``` r
# Generate sample data
set.seed(123)
data <- rnorm(100, mean = 5, sd = 2)

# Define the problem (separate from solver strategy)
problem <- mle_problem(
  loglike = function(theta) {
    if (theta[2] <= 0) return(-Inf)
    sum(dnorm(data, theta[1], theta[2], log = TRUE))
  },
  score = function(theta) {
    mu <- theta[1]; sigma <- theta[2]; n <- length(data)
    c(sum(data - mu) / sigma^2,
      -n / sigma + sum((data - mu)^2) / sigma^3)
  },
  constraint = mle_constraint(
    support = function(theta) theta[2] > 0,
    project = function(theta) c(theta[1], max(theta[2], 1e-6))
  ),
  theta_names = c("mu", "sigma")
)

# Solve with gradient ascent
result <- gradient_ascent()(problem, theta0 = c(0, 1))

cat("Estimated mean:", result$theta.hat[1], "(true: 5)\n")
#> Estimated mean: 5.180812 (true: 5)
cat("Estimated sd:", result$theta.hat[2], "(true: 2)\n")
#> Estimated sd: 1.816481 (true: 2)
```

## The Problem-Solver Separation

The key design principle is separating **what** you’re estimating from
**how** you estimate it:

``` r
# The problem encapsulates the statistical model
print(problem)
#> MLE Problem
#>   Parameters: mu, sigma 
#>   Score: analytic 
#>   Fisher: numerical 
#>   Constraints: yes

# Solvers are independent strategies
solver1 <- gradient_ascent(max_iter = 100)
solver2 <- newton_raphson(max_iter = 50)
solver3 <- bfgs()

# Same problem, different solvers
result1 <- solver1(problem, c(0, 1))
result2 <- solver2(problem, c(0, 1))
result3 <- solver3(problem, c(0, 1))

cat("Gradient ascent:", result1$theta.hat, "\n")
#> Gradient ascent: 5.180812 1.816481
cat("Newton-Raphson:", result2$theta.hat, "\n")
#> Newton-Raphson: 0 1
cat("BFGS:", result3$theta.hat, "\n")
#> BFGS: 100.7711 567.4039
```

## Composing Solvers

### Sequential Chaining (`%>>%`)

Chain solvers for coarse-to-fine optimization:

``` r
# Grid search finds a good region, then gradient ascent refines
strategy <- grid_search(lower = c(-10, 0.5), upper = c(10, 5), n = 5) %>>%
  gradient_ascent(max_iter = 50)

result <- strategy(problem, theta0 = c(0, 1))
cat("Result:", result$theta.hat, "\n")
#> Result: 5.180812 1.816481
```

### Three-Stage Refinement

``` r
# Coarse grid -> gradient ascent -> Newton-Raphson polish
strategy <- grid_search(lower = c(-10, 0.5), upper = c(10, 5), n = 5) %>>%
  gradient_ascent(max_iter = 30) %>>%
  newton_raphson(max_iter = 10)

result <- strategy(problem, theta0 = c(0, 1))
cat("Result:", result$theta.hat, "\n")
#> Result: 5.180812 1.816481
cat("Converged:", result$converged, "\n")
#> Converged: FALSE
```

### Parallel Racing (`%|%`)

Race multiple methods and keep the best result:

``` r
# Try gradient-based and derivative-free methods
strategy <- gradient_ascent() %|% bfgs() %|% nelder_mead()

result <- strategy(problem, theta0 = c(0, 1))
cat("Winner:", result$solver, "\n")
#> Winner: gradient_ascent
cat("Result:", result$theta.hat, "\n")
#> Result: 5.180812 1.816481
```

### Random Restarts

Escape local optima with multiple starting points:

``` r
strategy <- with_restarts(
  gradient_ascent(),
  n = 10,
  sampler = uniform_sampler(c(-10, 0.5), c(10, 5))
)

result <- strategy(problem, theta0 = c(0, 1))
cat("Best restart:", result$best_restart, "of", result$n_restarts, "\n")
#> Best restart: 1 of 10
cat("Result:", result$theta.hat, "\n")
#> Result: 5.180812 1.816481
```

### Conditional Refinement

Only refine if the first solver didn’t converge:

``` r
strategy <- unless_converged(
  gradient_ascent(max_iter = 10),  # Quick attempt

  newton_raphson(max_iter = 50)     # Refine if needed
)

result <- strategy(problem, theta0 = c(0, 1))
cat("Result:", result$theta.hat, "\n")
#> Result: 5.180812 1.816481
```

## Available Solvers

| Factory                                                                                          | Method                           | Requires                     |
|--------------------------------------------------------------------------------------------------|----------------------------------|------------------------------|
| [`gradient_ascent()`](https://queelius.github.io/compositional.mle/reference/gradient_ascent.md) | Steepest ascent with line search | score (or numerical)         |
| [`newton_raphson()`](https://queelius.github.io/compositional.mle/reference/newton_raphson.md)   | Second-order Newton              | score, fisher (or numerical) |
| [`bfgs()`](https://queelius.github.io/compositional.mle/reference/bfgs.md)                       | Quasi-Newton BFGS                | score (or numerical)         |
| [`lbfgsb()`](https://queelius.github.io/compositional.mle/reference/lbfgsb.md)                   | L-BFGS-B with box constraints    | score (or numerical)         |
| [`nelder_mead()`](https://queelius.github.io/compositional.mle/reference/nelder_mead.md)         | Simplex (derivative-free)        | \-                           |
| [`grid_search()`](https://queelius.github.io/compositional.mle/reference/grid_search.md)         | Exhaustive grid                  | \-                           |
| [`random_search()`](https://queelius.github.io/compositional.mle/reference/random_search.md)     | Random sampling                  | \-                           |

## Constraints

Define domain constraints with support checking and projection:

``` r
# Positive parameters
pos_constraint <- mle_constraint(
  support = function(theta) all(theta > 0),
  project = function(theta) pmax(theta, 1e-8)
)

# Box constraints [0, 10]
box_constraint <- mle_constraint(
  support = function(theta) all(theta >= 0 & theta <= 10),
  project = function(theta) pmax(0, pmin(10, theta))
)

# Use in problem definition
problem_constrained <- mle_problem(
  loglike = function(theta) -sum((theta - 5)^2),
  constraint = pos_constraint
)
```

## Function Transformers

### Stochastic Gradient (Mini-batching)

For large datasets, subsample observations:

``` r
# Original log-likelihood uses all data
loglike_full <- function(theta, obs = large_data) {
  sum(dnorm(obs, theta[1], theta[2], log = TRUE))
}

# Stochastic version uses random subsets
loglike_sgd <- with_subsampling(loglike_full, data = large_data, subsample_size = 100)
```

### Regularization

Add penalty terms for regularization:

``` r
loglike <- function(theta) -sum(theta^2)

# L1 (Lasso), L2 (Ridge), Elastic Net
loglike_l1 <- with_penalty(loglike, penalty_l1(), lambda = 0.1)
loglike_l2 <- with_penalty(loglike, penalty_l2(), lambda = 0.1)
loglike_enet <- with_penalty(loglike, penalty_elastic_net(alpha = 0.5), lambda = 0.1)

theta <- c(1, 2, 3)
cat("Original:", loglike(theta), "\n")
#> Original: -14
cat("With L1:", loglike_l1(theta), "\n")
#> With L1: -14.6
cat("With L2:", loglike_l2(theta), "\n")
#> With L2: -15.4
```

## Tracing Optimization

Track the optimization path for diagnostics:

``` r
trace_config <- mle_trace(values = TRUE, path = TRUE, gradients = TRUE)

result <- gradient_ascent(max_iter = 20)(
  problem,
  theta0 = c(0, 1),
  trace = trace_config
)

if (!is.null(result$trace_data)) {
  cat("Iterations:", result$trace_data$total_iterations, "\n")
  cat("Final log-likelihood:", tail(result$trace_data$values, 1), "\n")
}
#> Iterations: 20 
#> Final log-likelihood: -201.5839
```

## API Summary

**Problem Specification:** -
[`mle_problem()`](https://queelius.github.io/compositional.mle/reference/mle_problem.md) -
Define the estimation problem -
[`mle_constraint()`](https://queelius.github.io/compositional.mle/reference/mle_constraint.md) -
Domain constraints

**Solver Factories:** -
[`gradient_ascent()`](https://queelius.github.io/compositional.mle/reference/gradient_ascent.md),
[`newton_raphson()`](https://queelius.github.io/compositional.mle/reference/newton_raphson.md),
[`bfgs()`](https://queelius.github.io/compositional.mle/reference/bfgs.md),
[`lbfgsb()`](https://queelius.github.io/compositional.mle/reference/lbfgsb.md),
[`nelder_mead()`](https://queelius.github.io/compositional.mle/reference/nelder_mead.md) -
[`grid_search()`](https://queelius.github.io/compositional.mle/reference/grid_search.md),
[`random_search()`](https://queelius.github.io/compositional.mle/reference/random_search.md)

**Composition:** - `%>>%` - Sequential chaining - `%|%` - Parallel
racing -
[`with_restarts()`](https://queelius.github.io/compositional.mle/reference/with_restarts.md) -
Multiple starting points -
[`unless_converged()`](https://queelius.github.io/compositional.mle/reference/unless_converged.md) -
Conditional refinement -
[`compose()`](https://queelius.github.io/compositional.mle/reference/compose.md) -
Compose multiple solvers

**Samplers:** -
[`uniform_sampler()`](https://queelius.github.io/compositional.mle/reference/uniform_sampler.md),
[`normal_sampler()`](https://queelius.github.io/compositional.mle/reference/normal_sampler.md)

**Transformers:** -
[`with_subsampling()`](https://queelius.github.io/compositional.mle/reference/with_subsampling.md),
[`with_penalty()`](https://queelius.github.io/compositional.mle/reference/with_penalty.md) -
[`penalty_l1()`](https://queelius.github.io/compositional.mle/reference/penalty_l1.md),
[`penalty_l2()`](https://queelius.github.io/compositional.mle/reference/penalty_l2.md),
[`penalty_elastic_net()`](https://queelius.github.io/compositional.mle/reference/penalty_elastic_net.md)
