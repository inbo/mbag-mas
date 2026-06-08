simulate_mas_data <- function(design, ...) {
  stopifnot(require("glmmTMB"))

  # Create dataframe
  df <- expand.grid(
    telpunt = seq_len(design$n_telpunten),
    jaar = seq_len(design$n_jaar) - 1
  )

  # Half of points is in SBP
  sbp_per_telpunt <- rep(
    c("buiten", "binnen"),
    length.out = design$n_telpunten
  )
  df$sbp <- sbp_per_telpunt[df$telpunt]
  df$sbp_num <- as.numeric(df$sbp == "binnen")

  # Create random effect for location
  rf_telpunt <- rnorm(
    design$n_telpunten,
    mean = 0,
    sd = design$sigma_punt
  )

  # Linear predictor
  df$eta <- design$beta_0 +
    design$beta_1 * df$jaar +
    design$beta_2 * df$sbp_num +
    design$beta_3 * df$jaar * df$sbp_num +
    rf_telpunt[df$telpunt]

  # Expected count
  df$mu <- exp(df$eta)

  # Observed count
  is_negbin <- "theta" %in% names(design)
  if (is_negbin) {
    df$count <- rnbinom(
      nrow(df),
      mu = df$mu,
      size = design$theta
    )
  } else {
    df$count <- rpois(
      nrow(df),
      lambda = df$mu
    )
  }
  df$sbp_f <- factor(df$sbp, levels = c("buiten", "binnen"))
  df$telpunt_f <- factor(df$telpunt)

  # Fit models
  if (is_negbin) {
    model <- try(
      glmmTMB(count ~ jaar * sbp_f + (1 | telpunt_f),
              data = df,
              family = nbinom2())
    )
  } else {
    model <- try(
      glmmTMB(count ~ jaar * sbp_f + (1 | telpunt_f),
              data = df,
              family = poisson())
    )
  }

  if (inherits(model, "try-error")) {
    return(1)
  }

  # Return p-value
  p <- summary(model)$coefficients$cond["jaar:sbp_fbinnen", "Pr(>|z|)"]
  p_final <- pmin(p, 1, na.rm = TRUE)

  return(p_final)
}
