tar_read_mas <- function(name, species = NULL, ...) {
  # Targets store
  mbag_dir <- rprojroot::find_root_file(criterion = rprojroot::is_git_root)
  store_path <- file.path(mbag_dir, "source", "targets", "power_mas_light",
                          "_targets")

  # Specify name
  if (!is.null(species)) {
    species <- gsub("\\s", ".", species)
    name <- paste0(name, paste0("_", species))
  }

  # Get target
  targets::tar_read_raw(name = name, ..., store = store_path)
}
