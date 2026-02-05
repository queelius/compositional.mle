# Create a Trace Configuration

Specifies what information to track during optimization.

## Usage

``` r
mle_trace(
  values = FALSE,
  path = FALSE,
  gradients = FALSE,
  timing = FALSE,
  every = 1L
)

# S3 method for class 'mle_trace'
print(x, ...)
```

## Arguments

- values:

  Track log-likelihood values at each iteration

- path:

  Track parameter values at each iteration

- gradients:

  Track gradient norms at each iteration

- timing:

  Track wall-clock time

- every:

  Record every nth iteration (1 = all iterations)

- x:

  An mle_trace object.

- ...:

  Additional arguments (unused).

## Value

An mle_trace configuration object

The input object, invisibly (for method chaining).

## Examples

``` r
# Track everything
trace <- mle_trace(values = TRUE, path = TRUE, gradients = TRUE)

# Minimal tracing (just convergence path)
trace <- mle_trace(values = TRUE)

# Sample every 10th iteration for long runs
trace <- mle_trace(values = TRUE, path = TRUE, every = 10)
```
