# Gradient Ascent Solver

Creates a solver that uses gradient ascent (steepest ascent) to find the
MLE. Optionally uses backtracking line search for adaptive step sizes.

## Usage

``` r
gradient_ascent(
  learning_rate = 1,
  line_search = TRUE,
  max_iter = 100L,
  tol = 1e-08,
  backtrack_ratio = 0.5,
  min_step = 1e-12,
  verbose = FALSE
)
```

## Arguments

- learning_rate:

  Base learning rate / maximum step size

- line_search:

  Use backtracking line search for adaptive step sizes

- max_iter:

  Maximum number of iterations

- tol:

  Convergence tolerance (on parameter change)

- backtrack_ratio:

  Step size reduction factor for line search (0 \< r \< 1)

- min_step:

  Minimum step size before giving up

- verbose:

  Logical; if TRUE and the cli package is installed, display progress
  during optimization. Default is FALSE.

## Value

A solver function with signature (problem, theta0, trace) -\> mle_result

## Details

Gradient ascent iteratively moves in the direction of the score
(gradient of log-likelihood). With line search enabled, the step size is
adaptively chosen to ensure the log-likelihood increases.

The solver respects constraints defined in the problem via projection.

## See also

[`newton_raphson`](https://queelius.github.io/compositional.mle/reference/newton_raphson.md)
for second-order optimization,
[`bfgs`](https://queelius.github.io/compositional.mle/reference/bfgs.md)
for quasi-Newton, `%>>%` and
[`%|%`](https://queelius.github.io/compositional.mle/reference/race_operator.md)
for solver composition

## Examples

``` r
# Create a solver with default parameters
solver <- gradient_ascent()

# Create a solver with custom parameters
solver <- gradient_ascent(
  learning_rate = 0.5,
  max_iter = 500,
  tol = 1e-10
)

# Without line search (fixed step size)
solver <- gradient_ascent(learning_rate = 0.01, line_search = FALSE)
```
