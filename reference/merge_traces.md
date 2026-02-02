# Merge trace data from multiple results

Concatenates trace data from a sequence of results (e.g., from composed
solvers). The merged trace preserves stage boundaries for later
analysis.

## Usage

``` r
merge_traces(results)
```

## Arguments

- results:

  List of mle_numerical results with trace_data

## Value

A merged mle_trace_data object with stage information
