# Sequential Solver Composition

Chains two solvers sequentially. The result of the first solver becomes
the starting point for the second. This enables coarse-to-fine
strategies.

## Usage

``` r
s1 %>>% s2
```

## Arguments

- s1:

  First solver function

- s2:

  Second solver function

## Value

A new solver function that runs s1 then s2

## Details

Trace data from all solvers in the chain is merged into a single trace
with stage boundaries preserved.

## Examples

``` r
# Coarse-to-fine: grid search to find good region, then gradient ascent
strategy <- grid_search(lower = c(-10, 0.1), upper = c(10, 5), n = 5) %>>%
  gradient_ascent()

# Three-stage refinement
strategy <- grid_search(lower = c(-10, 0.1), upper = c(10, 5), n = 3) %>>%
  gradient_ascent() %>>%
  newton_raphson()
```
