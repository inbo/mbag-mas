# Paths to raw data from SOVON
paths_to_counts_sovon <- function(
    proj_path,
    pattern = "qgis_export_sovon_wfs") {
  # List paths to all files
  file_paths <- list.files(
    file.path(proj_path, "data"),
    pattern = pattern,
    full.names = TRUE,
    recursive = TRUE)

  return(file_paths)
}

# Path to counting locations
path_to_samples <- function(proj_path, file) {
  file_path <- file.path(proj_path, "data", "steekproefkaders", file)
  return(file_path)
}

# Paths files crop use by year
paths_to_lbg_year <- function(
    proj_path,
    pattern = "Landbouwgebruikspercelen") {
  # List paths to all files
  file_paths <- list.files(
    file.path(proj_path, "data", "landbouwgebruikspercelen"),
    pattern = pattern,
    full.names = TRUE,
    recursive = TRUE)

  # Only return gpkg file paths
  indices <- grepl(pattern = ".gpkg$", file_paths)

  return(sort(file_paths[indices]))
}
