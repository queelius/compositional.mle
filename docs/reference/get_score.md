# Get score function from problem

Returns the score (gradient) function, computing numerically if not
provided. If `cache_derivatives = TRUE` was set in the problem and score
is computed numerically, results are cached using a single-value cache.

## Usage

``` r
get_score(problem)
```

## Arguments

- problem:

  An mle_problem object

## Value

Score function
