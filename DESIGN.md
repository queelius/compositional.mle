# compositional.mle Design Document

## Philosophy

Following SICP principles:
1. **Primitive expressions** - Basic solvers (gradient ascent, Newton-Raphson, simulated annealing, etc.)
2. **Means of combination** - Composition operators (`%>>%`, `%|%`, `with_restarts`)
3. **Means of abstraction** - Solver factories, problem specification

**Key property**: Closure - combining solvers yields a solver.

## Core Abstractions

### 1. mle_problem

Encapsulates the statistical estimation problem, separate from optimization strategy.

```r
problem <- mle_problem(
  loglike,
  score = NULL,           # Auto-computed via numDeriv if NULL
  fisher = NULL,          # Auto-computed via numDeriv if NULL
  constraint = NULL,      # mle_constraint object
  theta_names = NULL,     # Parameter names for nice output
  n_obs = NULL            # For AIC/BIC
)
```

**Key features**:
- Lazy numerical differentiation when analytic forms not provided
- Immutable - create new problems via `update(problem, ...)`
- Validates inputs on construction

### 2. Solver Functions

A solver is a function: `(problem, theta0, trace) -> mle_result`

Solver *factories* return solver functions:

```r
# Factory pattern
gradient_ascent <- function(
  learning_rate = 1.0,
  line_search = TRUE,
  max_iter = 100,
  tol = 1e-8
) {
  # Returns a solver function
  function(problem, theta0, trace = mle_trace()) {
    # ... optimization logic ...
    # Returns mle_result
  }
}
```

This means:
- `gradient_ascent()` returns a solver
- `gradient_ascent(max_iter = 200)` returns a configured solver
- All solvers have the same signature: `(problem, theta0, trace) -> result`

### 3. Composition Operators

**Sequential** (`%>>%`): Chain solvers, passing result as next starting point
```r
grid_search(lower, upper, n = 10) %>>% gradient_ascent() %>>% newton_raphson()
```

**Parallel/Race** (`%|%`): Run both, select best by log-likelihood
```r
gradient_ascent() %|% nelder_mead() %|% bfgs()
```

**Restarts**: Multiple starting points
```r
with_restarts(gradient_ascent(), n = 20, sampler = uniform_sampler(lower, upper))
```

**Conditional**:
```r
unless_converged(gradient_ascent(), newton_raphson())
```

### 4. Tracing System

```r
trace <- mle_trace(
  values = TRUE,      # Track log-likelihood
  path = TRUE,        # Track parameter values
  gradients = TRUE,   # Track gradient norms
  timing = TRUE       # Track wall-clock time
)

result <- solver(problem, theta0, trace = trace)

# Visualize
plot(result)                    # Convergence diagnostics
optimization_path(result)       # Data frame of path
```

### 5. Results: mle_numerical

Extends algebraic.mle::mle_numerical with:
- `$converged` - logical
- `$iterations` - count
- `$trace_data` - optimization trace (if requested)
- `$chain` - for composed solvers, list of intermediate results
- `$solver` - which solver produced this result
- `$strategy` - composition type ("sequential", "race", "restarts")

## Implemented Solvers

### Gradient-Based (use score, auto-computed if not provided)
| Factory | Description | Implementation |
|---------|-------------|----------------|
| `gradient_ascent()` | Steepest ascent with backtracking line search | Native |
| `bfgs()` | Quasi-Newton BFGS | `optim()` wrapper |
| `lbfgsb()` | Limited-memory BFGS with box constraints | `optim()` wrapper |

### Second-Order (use score + fisher, auto-computed if not provided)
| Factory | Description | Implementation |
|---------|-------------|----------------|
| `newton_raphson()` | Classic Newton-Raphson with line search | Native |
| `fisher_scoring()` | Alias for newton_raphson | Native |

### Derivative-Free
| Factory | Description | Implementation |
|---------|-------------|----------------|
| `nelder_mead()` | Simplex method | `optim()` wrapper |
| `grid_search()` | Exhaustive grid evaluation | Native |
| `random_search()` | Random sampling | Native |
| `sim_anneal()` | Simulated annealing with configurable cooling | Native |

### Coordinate Methods
| Factory | Description | Implementation |
|---------|-------------|----------------|
| `coordinate_ascent()` | Golden section search per coordinate | Native |

## File Organization

```
R/
  numerical.mle.R       # Package documentation, imports
  problem.R             # mle_problem(), update.mle_problem()
  config.R              # mle_constraint(), mle_config*()
  compose.R             # %>>%, %|%, with_restarts(), unless_converged(), samplers
  trace.R               # mle_trace(), recorder, finalize
  plot.R                # plot.mle_numerical(), optimization_path()
  transformers.R        # with_penalty(), with_subsampling(), penalty_*()
  generic_functions.R   # is_converged(), num_iterations(), is_mle_*()

  solver_gradient.R     # gradient_ascent()
  solver_newton.R       # newton_raphson(), fisher_scoring()
  solver_optim.R        # bfgs(), lbfgsb(), nelder_mead()
  solver_grid.R         # grid_search(), random_search()
  solver_annealing.R    # sim_anneal()
  solver_coordinate.R   # coordinate_ascent()
```

## Example Usage

```r
library(compositional.mle)

# Generate data
set.seed(42)
data <- rnorm(100, mean = 5, sd = 2)

# Define the problem
problem <- mle_problem(
  loglike = function(theta) {
    sum(dnorm(data, theta[1], theta[2], log = TRUE))
  },
  constraint = mle_constraint(
    support = function(theta) theta[2] > 0,
    project = function(theta) c(theta[1], max(theta[2], 1e-8))
  ),
  theta_names = c("mu", "sigma"),
  n_obs = length(data)
)

# Simple solve
result <- gradient_ascent()(problem, c(0, 1))

# Composed strategy: coarse to fine
strategy <-
  grid_search(lower = c(-10, 0.1), upper = c(10, 5), n = 5) %>>%
  gradient_ascent(max_iter = 50) %>>%
  newton_raphson(max_iter = 20)

result <- strategy(problem, c(0, 1))

# Robust global search
strategy <- with_restarts(
  gradient_ascent(),
  n = 10,
  sampler = uniform_sampler(c(-10, 0.1), c(10, 5))
)

result <- strategy(problem, c(0, 1))

# Race different methods
strategy <- gradient_ascent() %|% bfgs() %|% nelder_mead()
result <- strategy(problem, c(0, 1))

# With tracing and visualization
result <- gradient_ascent()(
  problem,
  c(0, 1),
  trace = mle_trace(path = TRUE, values = TRUE, gradients = TRUE)
)
plot(result)
optimization_path(result)
```

## Future Directions

1. **Parallel execution**: Make `%|%` actually run in parallel via `future` package
2. **Automatic differentiation**: Support autodiff packages for score/fisher
3. **Derivative caching**: Memoize numerical derivatives to avoid redundant computation
4. **Verbose output**: Progress bars via `cli` package for long-running optimizations
5. **Early stopping**: Callback-based termination for composed solvers
