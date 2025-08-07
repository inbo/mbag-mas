get_individuals_from_ds <- function(
  ds_model,
  measure = c("density", "abundance")
) {
  # Get measure
  measure <- match.arg(measure, c("density", "abundance"))

  # Get model summary
  summary_ds <- summary(ds_model)

  # Return results table
  if (measure == "density") return(summary_ds$dht$individuals$D)
  return(summary_ds$dht$individuals$N)
}
