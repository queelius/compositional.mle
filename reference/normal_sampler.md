# Normal Sampler Factory

Creates a sampler function for use with `with_restarts` that generates
normally distributed starting points around a center.

## Usage

``` r
normal_sampler(center, sd = 1)
```

## Arguments

- center:

  Mean of the normal distribution

- sd:

  Standard deviation (scalar or vector)

## Value

A sampler function

## Examples

``` r
sampler <- normal_sampler(c(0, 1), sd = c(5, 0.5))
strategy <- with_restarts(gradient_ascent(), n = 20, sampler = sampler)
```
