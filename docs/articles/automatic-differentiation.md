# Automatic Differentiation with dualr

## Why Automatic Differentiation?

Maximum likelihood estimation requires derivatives of the
log-likelihood: the **score** (gradient) for first-order methods and the
**Hessian** (or observed information) for second-order methods like
Newton-Raphson. There are three ways to supply these:

| Approach                            | Accuracy                                 | Effort             | Speed                    |
|-------------------------------------|------------------------------------------|--------------------|--------------------------|
| Numerical (`numDeriv`)              | $O\left( h^{2} \right)$ truncation error | None (default)     | Slow (extra evaluations) |
| Hand-coded analytic                 | Exact                                    | High (error-prone) | Fast                     |
| Automatic differentiation (`dualr`) | Exact (to machine precision)             | Low                | Moderate                 |

Following the SICP principle that *the specification should be separate
from the mechanism*, the log-likelihood function is the specification of
our statistical model. Derivatives are mechanical consequences of that
specification—they should be derived automatically rather than
hand-coded.

The [`dualr`](https://github.com/queelius/dualr) package provides
forward-mode automatic differentiation for R via dual numbers, giving
exact derivatives with minimal code changes.

## Setup

``` r
library(compositional.mle)
library(dualr)
#> 
#> Attaching package: 'dualr'
#> The following object is masked from 'package:stats':
#> 
#>     deriv
```

## The Three Approaches

Consider a Poisson model. The log-likelihood for $\lambda$ given data
$x_{1},\ldots,x_{n}$ is:

$$\ell(\lambda) = \left( \sum\limits_{i = 1}^{n}x_{i} \right)\log\lambda - n\lambda$$

``` r
set.seed(42)
x <- rpois(100, lambda = 3.5)
```

We define the log-likelihood once:

``` r
ll_poisson <- function(theta) {
  lambda <- theta[1]
  n <- length(x)
  # Accumulate sum in a loop for dualr compatibility
  sx <- 0
  for (i in seq_along(x)) sx <- sx + x[i]
  sx * log(lambda) - n * lambda
}
```

Now we create three problem specifications using different derivative
strategies:

``` r
# 1. No derivatives — numDeriv fallback (the default)
p_numerical <- mle_problem(loglike = ll_poisson)

# 2. Hand-coded analytical derivatives
p_analytic <- mle_problem(
  loglike = ll_poisson,
  score = function(theta) sum(x) / theta[1] - length(x),
  fisher = function(theta) matrix(sum(x) / theta[1]^2, 1, 1)
)

# 3. Automatic differentiation via dualr
p_ad <- mle_problem(
  loglike = ll_poisson,
  score = function(theta) dualr::score(ll_poisson, theta),
  fisher = function(theta) dualr::observed_information(ll_poisson, theta)
)
```

``` r
p_numerical
#> MLE Problem
#>   Parameters: unnamed 
#>   Score: numerical 
#>   Fisher: numerical 
#>   Constraints: yes
p_ad
#> MLE Problem
#>   Parameters: unnamed 
#>   Score: analytic 
#>   Fisher: analytic 
#>   Constraints: yes
```

Notice that both the AD and analytic problems report “analytic” score
and Fisher information. From the solver’s perspective, they are
identical—the solver doesn’t know (or care) whether the derivatives were
hand-coded or computed by AD. This is the power of the `mle_problem`
abstraction: derivatives are pluggable.

## Comparison

Let’s run the same solver on all three:

``` r
solver <- bfgs(max_iter = 200)

r_num <- solver(p_numerical, theta0 = 1)
r_ana <- solver(p_analytic, theta0 = 1)
r_ad  <- solver(p_ad, theta0 = 1)

results <- data.frame(
  Method     = c("numDeriv", "Analytic", "dualr AD"),
  Estimate   = c(r_num$theta.hat, r_ana$theta.hat, r_ad$theta.hat),
  LogLik     = c(r_num$loglike, r_ana$loglike, r_ad$loglike),
  Converged  = c(r_num$converged, r_ana$converged, r_ad$converged),
  Iterations = c(r_num$iterations, r_ana$iterations, r_ad$iterations)
)
results
#>     Method Estimate   LogLik Converged Iterations
#> 1 numDeriv     3.64 106.2821      TRUE         21
#> 2 Analytic     3.64 106.2821      TRUE         23
#> 3 dualr AD     3.64 106.2821      TRUE         23
```

``` r
cat("True MLE (sample mean):", mean(x), "\n")
#> True MLE (sample mean): 3.64
```

All three converge to the same estimate. The key differences are:

- **numDeriv** introduces small $O\left( h^{2} \right)$ errors in the
  gradient, which may cause minor differences in iteration paths
  (usually negligible)
- **Analytic** and **dualr AD** provide exact gradients, leading to
  identical optimization paths
- **dualr AD** requires no manual derivative derivation

## Multivariate Example: Normal Distribution

For the Normal$(\mu,\sigma)$ model, we have two parameters and a
positivity constraint on $\sigma$. The log-likelihood is:

$$\ell(\mu,\sigma) = - n\log\sigma - \frac{1}{2\sigma^{2}}\sum\limits_{i = 1}^{n}\left( y_{i} - \mu \right)^{2}$$

``` r
set.seed(123)
y <- rnorm(200, mean = 5, sd = 2)
```

``` r
# Log-likelihood with loop-based sum for dualr Hessian support
ll_normal <- function(theta) {
  mu <- theta[1]
  sigma <- theta[2]
  n <- length(y)
  ss <- 0
  for (i in seq_along(y)) {
    ss <- ss + (y[i] - mu)^2
  }
  -n * log(sigma) - 0.5 * ss / sigma^2
}
```

**Note:** We use a loop-based sum instead of vectorized
`sum((y - mu)^2)`. This is because `dualr`’s Hessian computation
currently requires scalar accumulation when mixing dual numbers with
data vectors. The score function works either way, but the Hessian needs
this pattern. This is a minor syntactic cost for exact second
derivatives.

``` r
# AD-powered problem with box constraints via L-BFGS-B
p_normal <- mle_problem(
  loglike = ll_normal,
  score = function(theta) dualr::score(ll_normal, theta),
  fisher = function(theta) dualr::observed_information(ll_normal, theta)
)

solver <- lbfgsb(lower = c(-Inf, 1e-4), upper = c(Inf, Inf))
result <- solver(p_normal, theta0 = c(0, 1))
```

``` r
cat("Estimated mu:   ", round(result$theta.hat[1], 4), "\n")
#> Estimated mu:    4.9829
cat("Estimated sigma:", round(result$theta.hat[2], 4), "\n")
#> Estimated sigma: 1.8816
cat("Converged:      ", result$converged, "\n")
#> Converged:       TRUE

# Compare with true MLE
cat("\nTrue MLE mu:   ", round(mean(y), 4), "\n")
#> 
#> True MLE mu:    4.9829
cat("True MLE sigma:", round(sd(y) * sqrt((length(y)-1)/length(y)), 4), "\n")
#> True MLE sigma: 1.8816
```

The Hessian matrix from dualr provides the full curvature information:

``` r
H <- dualr::hessian(ll_normal, result$theta.hat)
cat("Hessian at MLE:\n")
#> Hessian at MLE:
print(round(H, 2))
#>        [,1]    [,2]
#> [1,] -56.49    0.00
#> [2,]   0.00 -112.98

cat("\nObserved information at MLE:\n")
#> 
#> Observed information at MLE:
I_obs <- dualr::observed_information(ll_normal, result$theta.hat)
print(round(I_obs, 2))
#>       [,1]   [,2]
#> [1,] 56.49   0.00
#> [2,]  0.00 112.98

cat("\nApproximate standard errors (from observed information):\n")
#> 
#> Approximate standard errors (from observed information):
se <- sqrt(diag(solve(I_obs)))
cat("  SE(mu):   ", round(se[1], 4), "\n")
#>   SE(mu):    0.133
cat("  SE(sigma):", round(se[2], 4), "\n")
#>   SE(sigma): 0.0941
```

## Composition with AD

The real power emerges when combining AD with solver composition.
Because `dualr`-derived derivatives are just functions, they compose
seamlessly:

``` r
# Coarse grid search, then refine with L-BFGS-B
strategy <- grid_search(
  lower = c(0, 0.5), upper = c(10, 5), n = 5
) %>>% lbfgsb(lower = c(-Inf, 1e-4), upper = c(Inf, Inf))

result_composed <- strategy(p_normal, theta0 = c(0, 1))
cat("Grid -> L-BFGS-B estimate:",
    round(result_composed$theta.hat, 4), "\n")
#> Grid -> L-BFGS-B estimate: 4.9829 1.8816
cat("Converged:", result_composed$converged, "\n")
#> Converged: TRUE
```

## When to Use What

| Situation                               | Recommended                                                                              | Reason                                       |
|-----------------------------------------|------------------------------------------------------------------------------------------|----------------------------------------------|
| Quick prototyping                       | `numDeriv` (default)                                                                     | Zero effort, just write the log-likelihood   |
| Production / accuracy-critical          | `dualr`                                                                                  | Exact derivatives, minimal code overhead     |
| Maximum performance on simple models    | Hand-coded                                                                               | Avoids AD overhead for trivial derivatives   |
| Complex models (mixtures, hierarchical) | `dualr`                                                                                  | Hand-coding is error-prone and tedious       |
| Derivative-free problems                | [`nelder_mead()`](https://queelius.github.io/compositional.mle/reference/nelder_mead.md) | When the log-likelihood isn’t differentiable |

The general recommendation is to **start with the default** (no explicit
derivatives) during model development, then **switch to `dualr`** when
you need accuracy or performance for production use. Hand-coding is only
worthwhile for very simple models where the derivatives are obvious.
