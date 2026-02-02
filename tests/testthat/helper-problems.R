# Shared test problem constructors
# Loaded automatically by testthat before any test file runs.

# Normal MLE problem with analytic score and optional fisher
make_normal_problem <- function(n = 100, true_mu = 5, true_sigma = 2,
                                seed = 42, fisher = FALSE) {
  set.seed(seed)
  data <- rnorm(n, mean = true_mu, sd = true_sigma)

  fisher_fn <- if (fisher) {
    function(theta) {
      sigma <- theta[2]
      n <- length(data)
      matrix(c(n / sigma^2, 0, 0, 2 * n / sigma^2), nrow = 2)
    }
  }

  mle_problem(
    loglike = function(theta) {
      if (theta[2] <= 0) return(-Inf)
      sum(dnorm(data, theta[1], theta[2], log = TRUE))
    },
    score = function(theta) {
      mu <- theta[1]
      sigma <- theta[2]
      n <- length(data)
      d_mu <- sum(data - mu) / sigma^2
      d_sigma <- -n / sigma + sum((data - mu)^2) / sigma^3
      c(d_mu, d_sigma)
    },
    fisher = fisher_fn,
    constraint = mle_constraint(
      support = function(theta) theta[2] > 0,
      project = function(theta) c(theta[1], max(theta[2], 1e-6))
    ),
    theta_names = c("mu", "sigma"),
    n_obs = n
  )
}

# Optim-style mle_numerical for testing generic functions
create_test_mle_numerical <- function(par, value, convergence = 0L,
                                       iterations = 50L, hessian = NULL) {
  sol <- list(
    par = par,
    value = value,
    convergence = convergence,
    hessian = hessian
  )
  result <- algebraic.mle::mle_numerical(sol = sol)
  result$iterations <- iterations
  result
}
