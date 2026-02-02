# Extract Optimization Path as Data Frame

Converts the trace data from an MLE result into a tidy data frame for
custom analysis and plotting (e.g., with ggplot2).

## Usage

``` r
optimization_path(x, ...)
```

## Arguments

- x:

  An mle_numerical result object with trace_data, or an mle_trace_data
  object

- ...:

  Additional arguments (unused)

## Value

A data frame with columns:

- `iteration`: Iteration number

- `loglike`: Log-likelihood value (if traced)

- `grad_norm`: Gradient norm (if traced)

- `time`: Elapsed time in seconds (if traced)

- `theta_1`, `theta_2`, ...: Parameter values (if path traced)

## Examples

``` r
# \donttest{
# Get optimization path as data frame
problem <- mle_problem(
  loglike = function(theta) -sum((theta - c(3, 2))^2),
  constraint = mle_constraint(support = function(theta) TRUE)
)
trace_cfg <- mle_trace(values = TRUE, path = TRUE)
result <- gradient_ascent(max_iter = 30)(problem, c(0, 0), trace = trace_cfg)

path_df <- optimization_path(result)
head(path_df)
#>   iteration      loglike   theta_1   theta_2
#> 1         1 -13.00000000 0.0000000 0.0000000
#> 2         2  -6.78889745 0.8320503 0.5547002
#> 3         3  -2.57779490 1.6641006 1.1094004
#> 4         4  -0.36669235 2.4961509 1.6641006
#> 5         5  -0.15558980 3.3282012 2.2188008
#> 6         6  -0.01114107 2.9121760 1.9414507
# }
```
