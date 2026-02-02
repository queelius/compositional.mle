# Multiple Random Restarts

Runs a solver from multiple starting points and returns the best result.
Essential for problems with multiple local optima.

## Usage

``` r
with_restarts(solver, n, sampler, max_reject = 100L)
```

## Arguments

- solver:

  A solver function

- n:

  Number of restarts (including the provided theta0)

- sampler:

  Function that generates random starting points. Called with no
  arguments, should return a parameter vector. Samples are automatically
  constrained using problem\$constraint.

- max_reject:

  Maximum rejection attempts per sample before projection

## Value

A new solver function with restart capability

## Details

The sampler generates candidate starting points, which are automatically
filtered/projected using the problem's constraint. This means samplers
can be simple distributions without constraint awareness.

## Examples

``` r
# 20 random restarts - constraint applied automatically from problem
sampler <- uniform_sampler(c(-10, 0), c(10, 5))
strategy <- with_restarts(gradient_ascent(), n = 20, sampler = sampler)

# Can also compose with other operators
strategy <- with_restarts(gradient_ascent(), n = 10, sampler = sampler) %>>%
  newton_raphson()
```
