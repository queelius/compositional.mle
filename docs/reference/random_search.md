# Random Search Solver

Creates a solver that evaluates the log-likelihood at random points and
returns the best. Useful for high-dimensional problems where grid search
is infeasible.

## Usage

``` r
random_search(sampler, n = 100L)
```

## Arguments

- sampler:

  Function generating random parameter vectors

- n:

  Number of random points to evaluate

## Value

A solver function

## Details

Unlike grid search, random search scales better to high dimensions. The
sampler should generate points in a reasonable region; points outside
the problem's constraint support are skipped.

## Examples

``` r
# Create a random search solver with uniform sampling
solver <- random_search(
  sampler = uniform_sampler(c(-10, 0.1), c(10, 5)),
  n = 100
)
```
