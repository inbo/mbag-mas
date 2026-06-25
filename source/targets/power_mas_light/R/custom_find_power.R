custom_find_power <- function(
  design,
  design_digits,
  ...,
  db_file,
  seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  designpower::find_power(
    design = design[-which(names(design) == "tar_group")],
    design_digits = design_digits,
    ...,
    filename = db_file
  )
}
