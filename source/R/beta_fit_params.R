#' Characterize Beta Distribution Based on Mean and Standard Deviation.
#'
#' @param beta_fun A Beta distribution function. One of `dbeta`, `pbeta`,
#' `qbeta`, `rbeta`.
#' @param mean Mean value.
#' @param sd Standard deviation.
#' @param ... Additional arguments passed to the Beta distribution function.
#'
#' @examples
#' # Set seed for reproducibility
#' set.seed(123)
#'
#' # Sample random numbers from Beta distribution
#' mean <- 0.6
#' sd <- 0.1
#' values <- beta_fit_params(rbeta, mean, sd, n = 10000)
#'
#' # Visualise distribution
#' hist(values)

beta_fit_params <- function(beta_fun, mean, sd, ...) {
  # Objective function to minimize
  # This calculates the difference between the target values (mean, sd), and the
  # values produced by the Beta distribution with the current parameters
  objective <- function(params) {
    shape1 <- params[1] # shape1 parameter (α)
    shape2 <- params[2]  # shape2 parameter (β)

    # Calculate the mean based on the current parameters
    calc_mean <- shape1 / (shape1 + shape2)
    # Calculate the sd based on the current parameters
    calc_sd <- sqrt(
      (shape1 * shape2) / ((shape1 + shape2)^2 * (shape1 + shape2 + 1))
    )

    # Minimize the error between target and calculated values
    # Error is the squared difference for each target value
    (calc_mean - mean)^2 + (calc_sd - sd)^2
  }

  # Initial guesses for shape1 and shape2 parameters
  start <- c(1, 1)

  # Optimize the parameters using the "L-BFGS-B" method
  # Lower bounds are set to prevent negative or zero shape parameters
  result <- tryCatch(
    optim(start, objective, method = "L-BFGS-B", lower = c(0.01, 0.01)),
    error = function(e) NULL
  )

  if (is.null(result) || result$convergence != 0) {
    # Return NA vector
    return(NA)
  } else {
    return(beta_fun(shape1 = result$par[1], shape2 = result$par[2], ...))
  }
}
