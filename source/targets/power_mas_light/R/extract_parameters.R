extract_parameters <- function(model) {
  summary_model <- summary(model)
  coef <- summary_model$coefficients$cond

  list(
    beta_0 = coef["(Intercept)", "Estimate"],
    beta_1 = coef["year2", "Estimate"],
    beta_2 = coef["sbp_fbinnen", "Estimate"],
    beta_3 = coef["year2:sbp_fbinnen", "Estimate"],
    sigma_punt = unname(attr(summary_model$varcor$cond$plotnaam_f, "stddev")),
    theta = exp(unname(model$fit$par["betadisp"]))
  )
}
