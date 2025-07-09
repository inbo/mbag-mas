# Paths to raw data from SOVON
paths_to_counts_sovon <- function(
    proj_path,
    pattern = "qgis_export_sovon_wfs") {
  # List paths to all files
  file_paths <- list.files(
    file.path(proj_path, "data"),
    pattern = pattern,
    full.names = TRUE,
    recursive = TRUE
  )

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
    start_year = 2022) {

  # List paths to files
  file_paths <- list.files(
    file.path(proj_path, "data", "landbouwgebruikspercelen"),
    pattern = "Landbouwgebruikspercelen",
    full.names = TRUE,
    recursive = TRUE
  )

  # Select geopackage files
  gpkg_files <- file_paths[grepl(pattern = ".gpkg$", file_paths)]

  # Select years
  year <- sapply(as.list(gpkg_files), function(file) {
    unique(as.numeric(
      stringr::str_extract_all(file, "[0-9]+")[[1]]
    ))
  })
  indices <- year >= start_year

  return(sort(gpkg_files[indices]))
}
