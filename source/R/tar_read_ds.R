tar_read_ds <- function(
  name,
  species = NULL,
  year = NULL,
  store = targets_store,
  start_year = 2023
) {
  # Specify name
  if (!is.null(species)) {
    species <- gsub("\\s", ".", species)
    name <- paste0(name, paste0("_", species))
  }

  # Specify branch
  if (is.null(year)) {
    branches <- NULL
  } else {
    branches <- (year - start_year) + 1
  }

  # Get target
  target <- targets::tar_read_raw(
    name = name,
    branches = branches,
    store = targets_store
  )

  if (length(year) == 1) return(target[[1]])
  return(target)
}
