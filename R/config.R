#' Create domain constraint specification
#'
#' Specifies domain constraints for optimization. The support function
#' checks if parameters are valid, and the project function maps invalid
#' parameters back to valid ones.
#'
#' @param support Function testing if theta is in support (returns TRUE/FALSE)
#' @param project Function projecting theta onto support
#' @return An mle_constraint object
#' @examples
#' # Positive parameters only
#' constraint <- mle_constraint(
#'   support = function(theta) all(theta > 0),
#'   project = function(theta) pmax(theta, 1e-8)
#' )
#'
#' # Parameters in [0, 1]
#' constraint <- mle_constraint(
#'   support = function(theta) all(theta >= 0 & theta <= 1),
#'   project = function(theta) pmax(0, pmin(1, theta))
#' )
#'
#' # No constraints (default)
#' constraint <- mle_constraint()
#' @export
mle_constraint <- function(
  support = function(theta) TRUE,
  project = function(theta) theta
) {
  stopifnot(
    is.function(support),
    is.function(project)
  )

  structure(
    list(
      support = support,
      project = project
    ),
    class = "mle_constraint"
  )
}

#' Check if object is an mle_constraint
#'
#' @param x Object to test
#' @return Logical indicating whether \code{x} is an \code{mle_constraint}.
#' @examples
#' constraint <- mle_constraint(support = function(theta) all(theta > 0))
#' is_mle_constraint(constraint)  # TRUE
#' is_mle_constraint(list())      # FALSE
#' @export
is_mle_constraint <- function(x) {
  inherits(x, "mle_constraint")
}
