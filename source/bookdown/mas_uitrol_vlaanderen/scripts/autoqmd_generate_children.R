#' Generate random QMD files from a template
#'
#' This function generates one or more Quarto (\code{.qmd}) files from a
#' template, filling in variables supplied via \code{...}. Each output file is
#' written to a specified directory and given a random unique name.
#'
#' @param ... Named vectors or lists of equal length, passed to
#'   \code{\link[knitr]{knit_expand}}. Each position across vectors represents
#'   one document to generate.
#' @param template Path to a Quarto template (\code{.qmd}) file to expand.
#' @param child_dir Directory where the generated files should be written.
#'   The directory is created if it does not exist.
#'
#' @details
#' Output filenames are generated using random hexadecimal strings, similar to
#' \code{tempfile()}, but always saved in the specified directory.
#' For example, files may look like:
#' \code{_qmd_a3f2b6d4.qmd}, \code{_qmd_7e19c8fa.qmd}, etc.
#'
#' @return Invisibly returns a character vector with the paths of the generated
#'   QMD files. The files are also written to disk.
autoqmd_generate_children <- function(
  ...,
  template,
  child_dir
) {
  # Capture inputs
  dots <- list(...)

  # Check that inputs are provided
  if (length(dots) == 0) {
    stop("You must provide at least one named argument via ...")
  }

  # Check equal lengths across arguments
  lens <- vapply(dots, length, integer(1))
  if (any(lens != lens[1])) {
    stop("All arguments in ... must have the same length")
  }

  # Determine how many QMD files to generate
  n <- lens[1]

  # Create directory if needed
  dir.create(child_dir, recursive = TRUE, showWarnings = FALSE)

  # Generate random file names
  random_names <- paste0(
    "_qmd_", sprintf("%08x", sample.int(16^7, n, replace = TRUE)), ".qmd"
  )
  out_files <- file.path(child_dir, random_names)

  # Generate and write files
  for (i in seq_len(n)) {
    args <- lapply(dots, function(x) x[i])
    do.call(knitr::knit_expand, c(list(template), args)) |>
      writeLines(out_files[i])
  }

  message("Generated ", n, " QMD file",
          if (n > 1) "s", " in '", child_dir, "'.")

  invisible(out_files)
}
