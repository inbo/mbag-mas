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

write_ipt_csv <- function(split_list, path = ".", suffix = "_mas") {
  # Validate input
  if (!all(c("events", "occurrences") %in% names(split_list))) {
    stop("Input must be a list with elements 'events' and 'occurrences'.")
  }

  # Create directory if needed
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  # Define output file paths
  event_file <- file.path(path, paste0("events", suffix, ".csv"))
  occ_file   <- file.path(path, paste0("occ", suffix, ".csv"))

  # Write files with UTF-8 encoding
  readr::write_csv(split_list$events, event_file, na = "")
  readr::write_csv(split_list$occurrences, occ_file, na = "")

  # Return named vector of paths
  return(c(events = event_file, occurrences = occ_file))
}
