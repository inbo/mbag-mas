create_output_csv <- function(x, file, path, suffix_by = NA, ...) {
  # Add suffix if provided
  if (!is.na(suffix_by)) {
    suffix <- paste(
      unique(c(min(x[suffix_by]), max(x[suffix_by]))),
      collapse = "_"
    )

    full_path <- paste0(file.path(path, file), "_", suffix, ".csv")
  } else {
    full_path <- paste0(file.path(path, file), ".csv")
  }

  # Create output path
  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  # Write out dataframe
  readr::write_csv(x, file = full_path, ...)

  # Return file path
  return(full_path)
}

create_output_gpkg <- function(x, file, path, suffix_by = NA, ...) {
  # Add suffix if provided
  if (!is.na(suffix_by)) {
    suffix <- paste(
      unique(c(min(st_drop_geometry(x)[suffix_by]),
               max(st_drop_geometry(x)[suffix_by]))),
      collapse = "_"
    )

    full_path <- paste0(file.path(path, file), "_", suffix, ".gpkg")
  } else {
    full_path <- paste0(file.path(path, file), ".gpkg")
  }

  # Create output path
  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  # Write out dataframe
  sf::st_write(x, dsn = full_path, ...)

  # Return file path
  return(full_path)
}
