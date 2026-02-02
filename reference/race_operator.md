# Parallel Solver Racing (Operator)

Runs multiple solvers and returns the best result (highest
log-likelihood). Useful when unsure which method will work best for a
given problem.

## Usage

``` r
s1 %|% s2
```

## Arguments

- s1:

  First solver function

- s2:

  Second solver function

## Value

A new solver function that runs both and picks the best

## Details

For parallel execution or more than 2 solvers, use
[`race`](https://queelius.github.io/compositional.mle/reference/race.md).

## See also

[`race`](https://queelius.github.io/compositional.mle/reference/race.md)
for parallel execution

## Examples

``` r
# Race gradient-based vs derivative-free
strategy <- gradient_ascent() %|% nelder_mead()

# Race multiple methods
strategy <- gradient_ascent() %|% bfgs() %|% nelder_mead()
```
