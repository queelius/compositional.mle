# compositional.mle: Compositional Maximum Likelihood Estimation

Provides composable optimization strategies for maximum likelihood
estimation (MLE). Solvers are first-class functions that combine via
sequential chaining, parallel racing, and random restarts. Implements
gradient ascent, Newton-Raphson, quasi-Newton (BFGS), and
derivative-free methods with support for constrained optimization and
tracing. Returns 'mle' objects compatible with 'algebraic.mle' for
downstream analysis.

A domain-specific language for maximum likelihood estimation where
solvers are first-class composable functions. Following SICP principles,
solvers combine via sequential chaining, parallel racing, and iteration
to build sophisticated optimization strategies from simple primitives.

## The Problem Abstraction

[`mle_problem`](https://queelius.github.io/compositional.mle/reference/mle_problem.md)
encapsulates a statistical estimation problem:

- Log-likelihood function

- Optional analytic score and Fisher information (computed numerically
  if not provided)

- Domain constraints

- Metadata (parameter names, observation count)

## Solver Factories

Solver factories return solver functions with signature
`(problem, theta0, trace) -> mle_result`:

- [`gradient_ascent`](https://queelius.github.io/compositional.mle/reference/gradient_ascent.md):
  First-order gradient method

- [`newton_raphson`](https://queelius.github.io/compositional.mle/reference/newton_raphson.md):
  Second-order Newton's method

- [`bfgs`](https://queelius.github.io/compositional.mle/reference/bfgs.md):
  Quasi-Newton BFGS

- [`nelder_mead`](https://queelius.github.io/compositional.mle/reference/nelder_mead.md):
  Derivative-free simplex

- [`grid_search`](https://queelius.github.io/compositional.mle/reference/grid_search.md):
  Exhaustive grid search

## Composition Operators

Combine solvers to build complex strategies:

- `%>>%`: Sequential chaining (coarse-to-fine)

- [`%|%`](https://queelius.github.io/compositional.mle/reference/race_operator.md):
  Parallel racing (try multiple, pick best)

- [`with_restarts`](https://queelius.github.io/compositional.mle/reference/with_restarts.md):
  Multiple random starting points

## Tracing

[`mle_trace`](https://queelius.github.io/compositional.mle/reference/mle_trace.md)
configures what to track during optimization (values, path, gradients,
timing) for diagnostics and visualization.

## See also

Useful links:

- <https://github.com/queelius/compositional.mle>

- <https://queelius.github.io/compositional.mle/>

- Report bugs at <https://github.com/queelius/compositional.mle/issues>

## Author

**Maintainer**: Alexander Towell <lex@metafunctor.com>
([ORCID](https://orcid.org/0000-0001-6443-9897))

## Examples

``` r
# \donttest{
# Define problem
set.seed(42)
x <- rnorm(100, mean = 5, sd = 2)
problem <- mle_problem(
  loglike = function(theta) sum(dnorm(x, theta[1], theta[2], log = TRUE)),
  constraint = mle_constraint(support = function(theta) theta[2] > 0,
                              project = function(theta) c(theta[1], max(theta[2], 1e-8)))
)

# Simple solve
result <- gradient_ascent()(problem, c(0, 1))

# Composed strategy: grid -> gradient -> Newton
strategy <- grid_search(lower = c(-10, 0.1), upper = c(10, 5), n = 5) %>>%
  gradient_ascent() %>>% newton_raphson()
result <- strategy(problem, c(0, 1))

# Race different methods
strategy <- gradient_ascent() %|% bfgs() %|% nelder_mead()
result <- strategy(problem, c(0, 1))
# }
```
