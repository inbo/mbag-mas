select_models <- function(aic_diff, model_list, n_models = 1, aic_tol = 2) {
  require("dplyr")
  require("rlang")

  selected_models <- aic_diff %>%
    arrange("`Delta AIC`") %>%
    filter(.data$`Delta AIC` <= aic_tol) %>%
    slice_min(.data$params, n = n_models) %>%
    pull("Model") %>%
    as.numeric()

  return(model_list[selected_models])
}
