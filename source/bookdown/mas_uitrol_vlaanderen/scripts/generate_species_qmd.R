#' Generate species-specific QMD files from a template
#'
#' This function generates one or more Quarto (\code{.qmd}) files from a
#' template, filling in species names and any additional variables supplied via
#' \code{...}. Each species produces a separate file in the target directory.
#'
#' @param species Character vector of species vernacular names.
#'   Each element generates one output QMD file.
#' @param ... Additional named vectors of the same length as \code{species}.
#'   These values are passed to the template via
#'   \code{\link[knitr]{knit_expand}}.
#'   For example, you may pass \code{sci_name} or \code{label}.
#' @param template Path to the QMD template to expand.
#'   Must be a valid input to \code{\link[knitr]{knit_expand}}.
#' @param suffix Optional character string appended to each output file name.
#'   Defaults to an empty string.
#' @param spec_dir Directory in which to write the output QMD files.
#'   Defaults to \code{"spec_files"}.
#'
#' @return Invisibly returns a character vector with the paths of the generated
#' files. The files are also written to disk.
generate_species_qmd <- function(
  species,
  ...,
  template,
  suffix = "",
  spec_dir = "spec_files"
) {
  # Capture additional arguments into a list
  dots <- list(...)

  # Ensure all additional vectors have the same length as 'species'
  lens <- vapply(dots, length, integer(1))
  if (length(lens) > 0 && any(lens != length(species))) {
    stop("All arguments in ... must have the same length as 'species'")
  }

  # Create the output directory if it does not already exist
  dir.create(spec_dir, recursive = TRUE, showWarnings = FALSE)

  # Loop over all species and generate a QMD file for each
  for (i in seq_along(species)) {
    spec <- species[i]

    # Create a safe lowercase + dot-separated label from the species name
    lab <- gsub("\\s", ".", tolower(spec))

    # Construct output file name, including optional suffix
    out_file <- paste0("_", lab, suffix, ".qmd")

    # Build argument list for knit_expand:
    # - Always include 'species'
    # - For each extra argument in '...', take its i-th element
    args <- c(
      list(species = spec),
      lapply(dots, function(x) x[i])
    )

    # Expand the template with current arguments and write to file
    do.call(knit_expand, c(list(template), args)) |>
      writeLines(file.path(spec_dir, out_file))
  }

  # Return (invisibly) the full paths of the generated QMD files
  invisible(
    file.path(
      spec_dir,
      paste0("_", gsub("\\s", ".", tolower(species)), suffix, ".qmd")
    )
  )
}
