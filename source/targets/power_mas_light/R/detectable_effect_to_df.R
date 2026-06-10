detectable_effect_to_df <- function(x) {
  res <- lapply(
    names(x),
    function(target_name) {
      value <- x[[target_name]]

      # Extract species and branch from target name
      parts <- strsplit(target_name, "_")[[1]]

      branch <- tail(parts, 1)
      species <- gsub("\\.", " ",
                      paste(parts[3:(length(parts) - 1)], collapse = "_"))

      data.frame(
        species = species,
        branch = branch,
        detectable_effect_low = value[2],
        detectable_effect = value[1],
        detectable_effect_high = value[3],
        as.data.frame(attr(value, "design")),
        check.names = FALSE
      )
    }
  )

  dplyr::bind_rows(res)
}
