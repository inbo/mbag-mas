get_det_probs <- function(model) {
  # Get predicted values
  preds <- predict(model, se.fit = TRUE)

  # Get detection curve covariates
  var_names <- all.vars(as.formula(model$ddf$ds$aux$ddfobj$scale$formula))

  # Create dataframe
  cbind(
    model$ddf$data,
    "estimate_p" = preds$fitted,
    "se_p" = preds$se
  ) %>%
    select(all_of(var_names), "estimate_p", "se_p") %>%
    distinct() %>%
    arrange(var_names) %>%
    rowwise() %>%
    mutate(
      ll_beta = beta_fit_params(
        beta_fun = qbeta,
        mean = .data$estimate_p,
        sd = .data$se_p,
        p = 0.025
      ),
      ul_beta = beta_fit_params(
        beta_fun = qbeta,
        mean = .data$estimate_p,
        sd = .data$se_p,
        p = 0.975
      )
    ) %>%
    ungroup()
}
