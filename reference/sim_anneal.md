# Simulated Annealing Solver

Creates a solver using simulated annealing for global optimization.
Simulated annealing can escape local optima by probabilistically
accepting worse solutions, with the acceptance probability decreasing
over time (controlled by a "temperature" parameter).

## Usage

``` r
sim_anneal(
  temp_init = 10,
  cooling_rate = 0.95,
  max_iter = 1000L,
  neighbor_sd = 1,
  min_temp = 1e-10,
  verbose = FALSE
)
```

## Arguments

- temp_init:

  Initial temperature (higher = more exploration)

- cooling_rate:

  Temperature reduction factor per iteration (0 \< r \< 1)

- max_iter:

  Maximum number of iterations

- neighbor_sd:

  Standard deviation for generating neighbor proposals

- min_temp:

  Minimum temperature before stopping

- verbose:

  Logical; if TRUE and the cli package is installed, display progress
  during optimization. Default is FALSE.

## Value

A solver function with signature (problem, theta0, trace) -\> mle_result

## Details

At each iteration: 1. Generate a neighbor by adding Gaussian noise to
current parameters 2. If the neighbor improves the objective, accept it
3. If the neighbor is worse, accept with probability exp(delta / temp)
4. Reduce temperature: temp = temp \* cooling_rate

The algorithm is stochastic and may find different solutions on
different runs. For best results, use with
[`with_restarts()`](https://queelius.github.io/compositional.mle/reference/with_restarts.md)
or combine with a local optimizer via `%>>%`.

## Examples

``` r
# Basic simulated annealing
solver <- sim_anneal()

# More exploration (higher initial temp, slower cooling)
solver <- sim_anneal(temp_init = 100, cooling_rate = 0.999)

# Coarse global search, then local refinement
strategy <- sim_anneal(max_iter = 500) %>>% gradient_ascent()
```
