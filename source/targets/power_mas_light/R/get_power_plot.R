get_power_plot <- function(
  design,
  design_digits,
  ...,
  db_file,
  seed = NULL
) {
  grDevices::recordPlot({
    custom_find_power(
      design = design,
      design_digits = design_digits,
      ...,
      db_file = db_file,
      seed = seed
    )
  })
}
