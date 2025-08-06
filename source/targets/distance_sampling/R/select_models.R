select_models <- function(aic_diff, model_list, aic_tol = 2) {
  require("dplyr")
  require("rlang")

  selected_model <- aic_diff %>%
    arrange("`Delta AIC`") %>%
    filter(.data$`Delta AIC` <= aic_tol) %>%
    slice_min(.data$params, n = 1) %>%
    pull("Model") %>%
    as.numeric()

  return(model_list[[selected_model]])
}
