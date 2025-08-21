select_ds_models <- function(aic_diff, model_list, aic_tol = 2) {
  require("dplyr")
  require("rlang")

  selected_model <- aic_diff %>%
    arrange("`Delta AIC`") %>%
    filter(.data$`Delta AIC` <= aic_tol) %>%
    # Lowest number of parameters within tolerance
    slice_min(.data$params, n = 1) %>%
    # In case of ties, choose lowest AIC
    slice_min(.data$`Delta AIC`, n = 1) %>%
    pull("Model")

  return(model_list[[selected_model]])
}
