# Package index

## Problem Specification

Define the statistical estimation problem

- [`mle_problem()`](https://queelius.github.io/compositional.mle/reference/mle_problem.md)
  : Create an MLE Problem Specification
- [`update(`*`<mle_problem>`*`)`](https://queelius.github.io/compositional.mle/reference/update.mle_problem.md)
  : Update an mle_problem
- [`is_mle_problem()`](https://queelius.github.io/compositional.mle/reference/is_mle_problem.md)
  : Check if object is an mle_problem
- [`get_score()`](https://queelius.github.io/compositional.mle/reference/get_score.md)
  : Get score function from problem
- [`get_fisher()`](https://queelius.github.io/compositional.mle/reference/get_fisher.md)
  : Get Fisher information function from problem
- [`mle_constraint()`](https://queelius.github.io/compositional.mle/reference/mle_constraint.md)
  : Create domain constraint specification
- [`is_mle_constraint()`](https://queelius.github.io/compositional.mle/reference/is_mle_constraint.md)
  : Check if object is an mle_constraint

## Solver Factories

Create solver functions

- [`gradient_ascent()`](https://queelius.github.io/compositional.mle/reference/gradient_ascent.md)
  : Gradient Ascent Solver
- [`newton_raphson()`](https://queelius.github.io/compositional.mle/reference/newton_raphson.md)
  : Newton-Raphson Solver
- [`fisher_scoring()`](https://queelius.github.io/compositional.mle/reference/fisher_scoring.md)
  : Fisher Scoring Solver
- [`bfgs()`](https://queelius.github.io/compositional.mle/reference/bfgs.md)
  : BFGS Solver
- [`lbfgsb()`](https://queelius.github.io/compositional.mle/reference/lbfgsb.md)
  : L-BFGS-B Solver (Box Constrained)
- [`nelder_mead()`](https://queelius.github.io/compositional.mle/reference/nelder_mead.md)
  : Nelder-Mead Solver (Derivative-Free)
- [`grid_search()`](https://queelius.github.io/compositional.mle/reference/grid_search.md)
  : Grid Search Solver
- [`random_search()`](https://queelius.github.io/compositional.mle/reference/random_search.md)
  : Random Search Solver

## Composition Operators

Combine solvers into strategies

- [`compose()`](https://queelius.github.io/compositional.mle/reference/compose.md)
  : Compose Multiple Solvers Sequentially
- [`` `%>>%` ``](https://queelius.github.io/compositional.mle/reference/grapes-greater-than-greater-than-grapes.md)
  : Sequential Solver Composition
- [`` `%|%` ``](https://queelius.github.io/compositional.mle/reference/grapes-or-grapes.md)
  : Parallel Solver Racing
- [`with_restarts()`](https://queelius.github.io/compositional.mle/reference/with_restarts.md)
  : Multiple Random Restarts
- [`unless_converged()`](https://queelius.github.io/compositional.mle/reference/unless_converged.md)
  : Conditional Refinement

## Samplers

Starting point generators for restarts

- [`uniform_sampler()`](https://queelius.github.io/compositional.mle/reference/uniform_sampler.md)
  : Uniform Sampler Factory
- [`normal_sampler()`](https://queelius.github.io/compositional.mle/reference/normal_sampler.md)
  : Normal Sampler Factory

## Function Transformers

Transform log-likelihood functions

- [`with_subsampling()`](https://queelius.github.io/compositional.mle/reference/with_subsampling.md)
  : Create stochastic log-likelihood with subsampling
- [`with_penalty()`](https://queelius.github.io/compositional.mle/reference/with_penalty.md)
  : Add penalty term to log-likelihood
- [`penalty_l1()`](https://queelius.github.io/compositional.mle/reference/penalty_l1.md)
  : L1 penalty function (LASSO)
- [`penalty_l2()`](https://queelius.github.io/compositional.mle/reference/penalty_l2.md)
  : L2 penalty function (Ridge)
- [`penalty_elastic_net()`](https://queelius.github.io/compositional.mle/reference/penalty_elastic_net.md)
  : Elastic net penalty (combination of L1 and L2)

## Configuration

Type-safe configuration objects

- [`mle_config()`](https://queelius.github.io/compositional.mle/reference/mle_config.md)
  : Create optimization configuration
- [`mle_config_gradient()`](https://queelius.github.io/compositional.mle/reference/mle_config_gradient.md)
  : Create gradient-based optimization configuration
- [`mle_config_linesearch()`](https://queelius.github.io/compositional.mle/reference/mle_config_linesearch.md)
  : Create line search configuration
- [`is_mle_config()`](https://queelius.github.io/compositional.mle/reference/is_mle_config.md)
  : Check if object is an mle_config

## Tracing

Track optimization progress

- [`mle_trace()`](https://queelius.github.io/compositional.mle/reference/mle_trace.md)
  : Create a Trace Configuration
- [`is_tracing()`](https://queelius.github.io/compositional.mle/reference/is_tracing.md)
  : Check if tracing is enabled

## Results

Work with optimization results

- [`is_converged()`](https://queelius.github.io/compositional.mle/reference/is_converged.md)
  : Check if solver converged
- [`is_mle_numerical()`](https://queelius.github.io/compositional.mle/reference/is_mle_numerical.md)
  : Check if object is an mle_numerical
- [`num_iterations()`](https://queelius.github.io/compositional.mle/reference/num_iterations.md)
  : Get number of iterations
