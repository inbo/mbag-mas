#' Write a Data Frame to a CSV File with Optional Suffix
#'
#' Creates a CSV file from a data frame and writes it to a specified directory.
#' Optionally appends a suffix based on the minimum and maximum values of a
#' specified column (useful for time-stamped or range-based outputs).
#'
#' @param x A data frame to write to disk.
#' @param file Character string specifying the base file name (without
#'  extension).
#' @param path Directory in which the file should be saved. Will be created
#'   recursively if it does not exist.
#' @param suffix_by Optional column name in `x`. If supplied, the output
#'   file name is suffixed with `"min_max"` values of this column. Use
#'   `NA` (default) to disable suffixing.
#' @param ... Additional arguments passed to `readr::write_csv()`.
#'
#' @return A character string giving the full path of the written CSV file.
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

#' Write Darwin Core Event and Occurrence Tables to CSV
#'
#' Writes a list containing Darwin Core `events` and `occurrences`
#' data frames to CSV files in IPT-compatible format. Output file names receive
#' a configurable suffix.
#'
#' @param split_list A list containing two data frames named `"events"`
#'   and `"occurrences"`.
#' @param path Directory where the CSV files should be created. Defaults to the
#'   current working directory. Will be created if it does not exist.
#' @param suffix Character string appended to the base output file names
#'   (default: `"_mas"`). Should begin with an underscore to separate it
#'   cleanly from the base names.
#'
#' @return A named character vector with the paths to the written CSV files:
#'   `c(events = "...", occurrences = "...")`.
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
