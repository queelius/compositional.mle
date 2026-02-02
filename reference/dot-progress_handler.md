# Create a progress handler for optimization

Create a progress handler for optimization

## Usage

``` r
.progress_handler(
  verbose = FALSE,
  solver_name = "Solver",
  max_iter = NULL,
  show_every = 1L
)
```

## Arguments

- verbose:

  Logical; whether to show progress

- solver_name:

  Name of the solver for display

- max_iter:

  Maximum iterations (for progress bar)

- show_every:

  Show progress every N iterations

## Value

A list with start(), update(), and finish() functions
