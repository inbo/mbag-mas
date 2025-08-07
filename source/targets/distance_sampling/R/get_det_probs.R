get_det_probs <- function(ds_model) {
  # Get predicted values
  preds <- predict(ds_model, se.fit = TRUE)

  # Get detection curve covariates
  var_names <- all.vars(as.formula(ds_model$ddf$ds$aux$ddfobj$scale$formula))

  # Create dataframe
  cbind(
    ds_model$ddf$data,
    "estimate_p" = preds$fitted,
    "se_p" = preds$se
  ) %>%
    select(all_of(var_names), "estimate_p", "se_p") %>%
    distinct() %>%
    arrange(var_names) %>%
    rowwise() %>%
    mutate(
      ll_beta = beta_fit_params( # nolint: object_usage_linter
        beta_fun = qbeta,
        mean = .data$estimate_p,
        sd = .data$se_p,
        p = 0.025
      ),
      ul_beta = beta_fit_params( # nolint: object_usage_linter
        beta_fun = qbeta,
        mean = .data$estimate_p,
        sd = .data$se_p,
        p = 0.975
      )
    ) %>%
    ungroup()
}
