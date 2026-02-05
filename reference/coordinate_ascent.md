# Coordinate Ascent Solver

Creates a solver that optimizes one parameter at a time while holding
others fixed. This is useful when parameters have different scales or
when the likelihood decomposes nicely along coordinate directions.

## Usage

``` r
coordinate_ascent(
  max_cycles = 50L,
  tol = 1e-08,
  line_search = TRUE,
  cycle_order = c("sequential", "random"),
  verbose = FALSE
)
```

## Arguments

- max_cycles:

  Maximum number of full cycles through all parameters

- tol:

  Convergence tolerance on log-likelihood change

- line_search:

  Use line search for each coordinate (slower but more robust)

- cycle_order:

  Order of cycling: "sequential" (1,2,...,p) or "random"

- verbose:

  Logical; if TRUE and the cli package is installed, display progress
  during optimization. Default is FALSE.

## Value

A solver function with signature (problem, theta0, trace) -\> mle_result

## Details

Each cycle consists of optimizing each coordinate in turn using a simple
golden section search. The algorithm converges when the log-likelihood
improvement in a full cycle is less than `tol`.

Coordinate ascent can be effective when:

- Parameters are on very different scales

- The likelihood has axis-aligned ridges

- Computing the full gradient is expensive

However, it may converge slowly for problems with strong parameter
correlations.

## See also

[`gradient_ascent`](https://queelius.github.io/compositional.mle/reference/gradient_ascent.md)
for gradient-based optimization,
[`nelder_mead`](https://queelius.github.io/compositional.mle/reference/nelder_mead.md)
for another derivative-free method

## Examples

``` r
# Basic coordinate ascent
solver <- coordinate_ascent()

# With more cycles for difficult problems
solver <- coordinate_ascent(max_cycles = 100)

# Random cycling to avoid systematic bias
solver <- coordinate_ascent(cycle_order = "random")
```
