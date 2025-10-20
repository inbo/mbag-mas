#' Anonymise observer names and maintain consistency across datasets
#'
#' Replaces observer names with consistent anonymous IDs (e.g. "Observer_001").
#' If a lookup table already exists, it ensures the same observers get the same
#' anonymous IDs in future runs. New observers get new IDs automatically.
#'
#' @param data A data frame containing a column with observer names.
#' @param observer_col Name of the column containing observer names. Will be
#' replaced with anonymous IDs in the output.
#' @param lookup_path Path to the CSV file storing the observer lookup table.
#' @param prefix Prefix used for anonymised IDs.
#'
#' @return The input data frame with the original observer column replaced by
#' anonymous IDs.
anonymise_observers <- function(
  data,
  observer_col,
  lookup_path,
  prefix
) {
  require("dplyr")
  require("readr")
  require("rlang")

  if (!observer_col %in% names(data)) {
    stop(paste("Column", observer_col, "not found in data."))
  }

  # Load or create lookup table
  observer_lookup <- if (file.exists(lookup_path)) {
    read_csv(lookup_path, show_col_types = FALSE)
  } else {
    tibble(recorded_by_temp = character(), observer_id = character())
  }

  # Identify new observers
  current_observers <- unique(data[[observer_col]])
  new_observers <- setdiff(current_observers, observer_lookup$recorded_by_temp)

  # Add new observers to lookup
  if (length(new_observers) > 0) {
    next_id <- nrow(observer_lookup) + 1
    new_lookup <- tibble(
      recorded_by_temp = new_observers,
      observer_id = sprintf(
        "%s%03d",
        prefix,
        seq(next_id, length.out = length(new_observers))
      )
    )
    observer_lookup <- bind_rows(observer_lookup, new_lookup)
    write_csv(observer_lookup, lookup_path)
  }

  # Join lookup to data
  data <- data %>%
    left_join(observer_lookup,
              by = join_by(!!observer_col == "recorded_by_temp")) %>%
    mutate("{observer_col}" := .data$observer_id) %>%
    select(-"observer_id")

  return(data)
}
