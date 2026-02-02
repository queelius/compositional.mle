# Golden section line search along one coordinate

Golden section line search along one coordinate

## Usage

``` r
.coordinate_line_search(
  loglike,
  theta,
  coord,
  constraint,
  max_iter = 50,
  tol = 1e-08
)
```

## Arguments

- loglike:

  Log-likelihood function

- theta:

  Current parameter vector

- coord:

  Index of coordinate to optimize

- constraint:

  Constraint object
