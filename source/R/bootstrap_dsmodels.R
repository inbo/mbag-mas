boot_statistic_ds <- function(data, indices, fun, ds_model, ...) {
  # Subset data and give unique IDs
  subset <- data[indices, ]
  subset$object <- rownames(subset)

  # Estimate detection probability
  ds_model <- ds(
    data = subset,
    key = ds_model$ddf$ds$aux$ddfobj$type,
    formula = as.formula(gsub("\\)", "",
                              gsub("^.*formula\\s=\\s", "",ds_model$ddf$model)[2])
                         ),
    adjustment = NULL,
    truncation = 300,
    transect = "point",
    dht_group = FALSE,
    quiet = TRUE)

  return(fun(ds_model, ...))
}

get_det_probs <- function(ds_model, group_vars) {
  cbind(ds_model$ddf$data,
        det_prob = predict(ds_model, se.fit = FALSE)$fitted) %>%
    distinct(!!sym(group_vars), det_prob) %>%
    arrange(!!sym(group_vars)) %>%
    pull(det_prob)
}

bootstrap_ds_model <- function(ds_model, samples, fun, group_vars) {
  require("boot")

  # Perform bootstrapping
  bootstrap_obj <- boot::boot(
    data = ds_model$ddf$data,
    statistic = boot_statistic_ds,
    R = samples,
    fun = get_det_probs,
    ds_model = ds_model,
    group_vars = group_vars)

  # Create dataframe of bootstrap samples
  colnames <- sort(unique(ds_model$ddf$data[[group_vars]]))
  out_df <- as_tibble(bootstrap_obj$t, .name_repair = "minimal")
  names(out_df) <- colnames

  return(out_df)
}
