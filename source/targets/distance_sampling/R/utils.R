add_categories <- function(df, ds_model, cats) {
  data <- ds_model$ddf$data

  stopifnot("Category name(s) not in data" = all(cats %in% names(data)))

  cbind(df, t(sapply(cats, function(i) unique(data[[i]]))))
}
