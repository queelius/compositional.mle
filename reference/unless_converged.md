# Conditional Refinement

Applies a refinement solver only if the first solver did not converge.
If refinement is applied, trace data from both solvers is merged.

## Usage

``` r
unless_converged(solver, refinement)
```

## Arguments

- solver:

  Primary solver function

- refinement:

  Solver to use if primary doesn't converge

## Value

A new solver function with conditional refinement

## Examples

``` r
# Use Newton-Raphson to refine if gradient ascent doesn't converge
strategy <- unless_converged(gradient_ascent(max_iter = 50), newton_raphson())
```
