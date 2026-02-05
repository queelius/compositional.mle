# Check if object is an mle_constraint

Check if object is an mle_constraint

## Usage

``` r
is_mle_constraint(x)
```

## Arguments

- x:

  Object to test

## Value

Logical indicating whether `x` is an `mle_constraint`.

## Examples

``` r
constraint <- mle_constraint(support = function(theta) all(theta > 0))
is_mle_constraint(constraint)  # TRUE
#> [1] TRUE
is_mle_constraint(list())      # FALSE
#> [1] FALSE
```
