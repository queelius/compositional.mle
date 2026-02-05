# Check if tracing is enabled

Check if tracing is enabled

## Usage

``` r
is_tracing(trace)
```

## Arguments

- trace:

  An mle_trace object

## Value

Logical indicating if any tracing is enabled

## Examples

``` r
# Tracing disabled (default)
trace <- mle_trace()
is_tracing(trace)  # FALSE
#> [1] FALSE

# Tracing enabled
trace <- mle_trace(values = TRUE)
is_tracing(trace)  # TRUE
#> [1] TRUE
```
