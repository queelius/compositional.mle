#' Create stochastic log-likelihood with subsampling
#'
#' Transforms a log-likelihood function to use only a random subsample of
#' observations. Useful for stochastic gradient ascent on large datasets.
#'
#' @param loglike Base log-likelihood function. Should accept theta and data.
#' @param data Observations (vector, matrix, or data.frame)
#' @param subsample_size Number of observations to sample per evaluation
#' @param replace Sample with replacement (logical, default: FALSE)
#' @return Transformed log-likelihood function
#' @examples
#' \dontrun{
#' # Original likelihood uses all data
#' data <- rnorm(10000, mean = 5, sd = 2)
#'
#' loglike <- function(theta, obs = data) {
#'   sum(dnorm(obs, mean = theta[1], sd = theta[2], log = TRUE))
#' }
#'
#' # Stochastic version uses random subsample
#' loglike_stoch <- with_subsampling(
#'   loglike,
#'   data = data,
#'   subsample_size = 100
#' )
#'
#' # Each call uses different random subsample
#' loglike_stoch(c(5, 2))
#' loglike_stoch(c(5, 2))  # Different value
#' }
#' @export
with_subsampling <- function(
  loglike,
  data,
  subsample_size,
  replace = FALSE
) {
  stopifnot(
    is.function(loglike),
    subsample_size > 0,
    is.logical(replace)
  )

  n_obs <- if (is.matrix(data) || is.data.frame(data)) {
    nrow(data)
  } else {
    length(data)
  }

  if (subsample_size > n_obs && !replace) {
    stop("subsample_size cannot exceed data size when replace=FALSE")
  }

  # Return a function with the same signature as loglike
  # but that uses subsampled data
  structure(
    function(theta) {
      # Sample data indices
      idx <- sample.int(n_obs, size = subsample_size, replace = replace)

      # Subset data appropriately
      if (is.matrix(data) || is.data.frame(data)) {
        subset_data <- data[idx, , drop = FALSE]
      } else {
        subset_data <- data[idx]
      }

      # Call original loglike with subsampled data
      loglike(theta, subset_data)
    },
    class = c("loglike_subsampled", "function"),
    base_loglike = loglike,
    subsample_size = subsample_size,
    n_obs = n_obs,
    replace = replace
  )
}

#' Add penalty term to log-likelihood
#'
#' Transforms a log-likelihood by subtracting a penalty term. Useful for
#' regularized estimation (e.g., LASSO, Ridge regression).
#'
#' @param loglike Base log-likelihood function
#' @param penalty Penalty function taking theta and returning numeric
#' @param lambda Penalty weight (non-negative numeric, default: 1.0)
#' @return Transformed log-likelihood function
#' @examples
#' \dontrun{
#' # Regression with L2 penalty (Ridge)
#' loglike <- function(theta) {
#'   # ... likelihood calculation ...
#' }
#'
#' # Add L2 penalty
#' loglike_penalized <- with_penalty(
#'   loglike,
#'   penalty = penalty_l2(),
#'   lambda = 0.1
#' )
#'
#' # Combine with stochastic subsampling
#' loglike_final <- loglike %>%
#'   with_subsampling(data, 100) %>%
#'   with_penalty(penalty_l1(), lambda = 0.01)
#' }
#' @export
with_penalty <- function(
  loglike,
  penalty,
  lambda = 1.0
) {
  stopifnot(
    is.function(loglike),
    is.function(penalty),
    is.numeric(lambda), lambda >= 0
  )

  structure(
    function(theta) {
      loglike(theta) - lambda * penalty(theta)
    },
    class = c("loglike_penalized", "function"),
    base_loglike = loglike,
    penalty = penalty,
    lambda = lambda
  )
}

#' L1 penalty function (LASSO)
#'
#' Creates a penalty function that computes the L1 norm (sum of absolute values).
#' Used for sparsity-inducing regularization.
#'
#' @param weights Optional parameter weights (default: all 1)
#' @return Penalty function
#' @examples
#' penalty <- penalty_l1()
#' penalty(c(1, -2, 3))  # Returns 6
#'
#' # Weighted L1
#' penalty <- penalty_l1(weights = c(1, 2, 1))
#' penalty(c(1, -2, 3))  # Returns 1*1 + 2*2 + 1*3 = 8
#' @export
penalty_l1 <- function(weights = NULL) {
  if (is.null(weights)) {
    function(theta) sum(abs(theta))
  } else {
    stopifnot(is.numeric(weights))
    function(theta) {
      stopifnot(length(theta) == length(weights))
      sum(abs(weights * theta))
    }
  }
}

#' L2 penalty function (Ridge)
#'
#' Creates a penalty function that computes the L2 norm squared (sum of squares).
#' Used for parameter shrinkage.
#'
#' @param weights Optional parameter weights (default: all 1)
#' @return Penalty function
#' @examples
#' penalty <- penalty_l2()
#' penalty(c(1, -2, 3))  # Returns 14
#'
#' # Weighted L2
#' penalty <- penalty_l2(weights = c(1, 2, 1))
#' penalty(c(1, -2, 3))  # Returns 1^2 + (2*2)^2 + 3^2 = 26
#' @export
penalty_l2 <- function(weights = NULL) {
  if (is.null(weights)) {
    function(theta) sum(theta^2)
  } else {
    stopifnot(is.numeric(weights))
    function(theta) {
      stopifnot(length(theta) == length(weights))
      sum((weights * theta)^2)
    }
  }
}

#' Elastic net penalty (combination of L1 and L2)
#'
#' Creates a penalty combining L1 and L2 norms. The parameter alpha controls
#' the balance: alpha=1 is pure LASSO, alpha=0 is pure Ridge.
#'
#' @param alpha Balance between L1 and L2 (numeric in [0,1], default: 0.5)
#' @param weights Optional parameter weights (default: all 1)
#' @return Penalty function
#' @examples
#' # Equal mix of L1 and L2
#' penalty <- penalty_elastic_net(alpha = 0.5)
#'
#' # More L1 (more sparsity)
#' penalty <- penalty_elastic_net(alpha = 0.9)
#'
#' # More L2 (more shrinkage)
#' penalty <- penalty_elastic_net(alpha = 0.1)
#' @export
penalty_elastic_net <- function(alpha = 0.5, weights = NULL) {
  stopifnot(is.numeric(alpha), alpha >= 0, alpha <= 1)

  l1_pen <- penalty_l1(weights)
  l2_pen <- penalty_l2(weights)

  function(theta) {
    alpha * l1_pen(theta) + (1 - alpha) * l2_pen(theta)
  }
}

#' Compose Multiple Function Transformations
#'
#' Applies transformations right-to-left (like mathematical composition).
#' This allows building complex log-likelihood transformations from simple ones.
#'
#' Note: For composing solvers, use \code{\link{compose}} instead.
#'
#' @param ... Transformer functions
#' @return Composed transformer function
#' @examples
#' \dontrun{
#' # Create a composition
#' transform <- compose_transforms(
#'   function(f) with_penalty(f, penalty_l1(), lambda = 0.01),
#'   function(f) with_subsampling(f, data, 50)
#' )
#'
#' # Apply to log-likelihood
#' loglike_transformed <- transform(loglike)
#'
#' # Equivalent to:
#' loglike_transformed <- loglike %>%
#'   with_subsampling(data, 50) %>%
#'   with_penalty(penalty_l1(), lambda = 0.01)
#' }
#' @export
compose_transforms <- function(...) {
  transforms <- list(...)

  if (length(transforms) == 0) {
    stop("At least one transformation required")
  }

  function(f) {
    result <- f
    # Apply right-to-left
    for (i in rev(seq_along(transforms))) {
      result <- transforms[[i]](result)
    }
    result
  }
}
