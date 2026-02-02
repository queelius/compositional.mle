# Uniform Sampler Factory

Creates a sampler function for use with `with_restarts` that generates
uniformly distributed starting points.

## Usage

``` r
uniform_sampler(lower, upper)
```

## Arguments

- lower:

  Lower bounds for each parameter

- upper:

  Upper bounds for each parameter

## Value

A sampler function

## Examples

``` r
sampler <- uniform_sampler(c(-10, 0.1), c(10, 5))
strategy <- with_restarts(gradient_ascent(), n = 20, sampler = sampler)
```
