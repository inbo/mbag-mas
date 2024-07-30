# Function to determine the structure of the data frames in the list
get_df_structure <- function(df_list) {
  require(dplyr)

  non_na_df <- df_list %>% compact() %>% first()
  empty_df <- purrr::map(non_na_df, ~ NA)
  empty_df <- as_tibble(empty_df)
  return(empty_df)
}

# Function to find the name of the element containing the value
find_df_name <- function(df_list, search_value) {
  require(dplyr)

  # Check if the value is present in each dataframe
  contains_value <- purrr::map_lgl(df_list, function(df) {
    vernacular_names <- df %>% pull(.data$vernacularName)

    any(grepl(paste0("^\\s*", search_value, "\\s*$"), vernacular_names,
              ignore.case = TRUE))
  })

  # Get the name of the first dataframe containing the value
  df_name <- names(df_list)[which(contains_value)][1]

  # Return the name
  return(df_name)
}

# https://gist.github.com/damianooldoni/3fa9cc1ffa67377a9757df097d48d19f
# input: a vernacular name
# output: the best matched scientific name from the GBIF Backbone
# NA_character_ if no match found
match_vernacular_name <- function(
    vernacular_name_df,
    filter_cols = NULL,
    ...) {
  vernacular_name <- pull(vernacular_name_df[1])

  # Lookup vernacular names in GBIF backbone
  gbif_lookup <- rgbif::name_lookup(
    vernacular_name,
    datasetKey = "d7dddbf4-2cf0-4f39-9b2a-bb099caae36c",
    ...)

  # Return taxon data frame if match found
  if (nrow(gbif_lookup$data) > 0) {
    # Define vernacular names and taxon data
    vernacular_names <- gbif_lookup$names
    taxon_data <- gbif_lookup$data

    # Use filter columns if provided
    if (!is.null(filter_cols)) {
      # In case of NA values
      cols_to_remove <- vernacular_name_df %>%
        select(where(~ any(is.na(.)))) %>%
        colnames()
      filter_cols <- filter_cols[!filter_cols %in% cols_to_remove]

      # Create the join condition dynamically
      join_condition <- stats::setNames(names(filter_cols),
                                        unlist(filter_cols))

      # Perform the inner join to select taxon data
      taxon_data <- vernacular_name_df %>%
        select(where(~ all(!is.na(.)))) %>%
        inner_join(taxon_data, by = join_condition) %>%
        select(-all_of(setdiff(colnames(vernacular_name_df), cols_to_remove)))

      # Use species keys to select vernacular names
      indices <- names(vernacular_names) %in% taxon_data$key
      vernacular_names <- vernacular_names[indices]
    }

    # Ifelse necessary??
    # Search taxon key in vernacular names if multiple possibilities
    if (nrow(taxon_data) > 1) {
      taxon_key <- find_df_name(vernacular_names, vernacular_name)

      # Return NA if no good match found
      if (length(taxon_key) == 0) {
        NA_character_
      # Return match with taxon key
      } else {
        subset(taxon_data, key == taxon_key)
      }

    # Return if only 1 match was found initially
    } else {
      taxon_data
    }

  # Return NA is no initial match found
  } else {
    NA_character_
  }
}

# Input dataframe with vernacular names and get taxon information
map_taxa_from_vernacular <- function(
    vernacular_name_df,
    vernacular_name_col,
    out_cols = "scientificName",
    filter_cols = NULL,
    ...) {
  require(dplyr)
  require(tidyr)

  group_cols <- unlist(filter_cols, use.names = FALSE)

  # Match vernacular names to get taxonomic info
  matched_names_df <- vernacular_name_df %>%

    # group  by vernacular name and compact the data
    group_by(across(all_of(c(vernacular_name_col, group_cols)))) %>%
    nest() %>%
    ungroup() %>%
    nest(match_df = all_of(c(vernacular_name_col, group_cols))) %>%

    # find scientific name for each (distinct) vernacular name
    mutate(taxon_df = purrr::map(
      match_df,
      match_vernacular_name,
      filter_cols = filter_cols,
      ...)) %>%
    unnest("match_df") %>%

    # Remove unneeded columns
    select(-"data")

  # Create output dataframe
  out_df <- matched_names_df %>%

    # Unnest taxon dataframes
    mutate(taxon_df = purrr::map(.data$taxon_df, ~ {
      # Unnesting only possible if all dataframes have the same structure
      if (all(is.na(.x))) {
        get_df_structure(matched_names_df$taxon_df)
      } else {
        .x
      }
    })
    ) %>%
    unnest("taxon_df") %>%
    ungroup() %>%

    # Add other columns from input df
    right_join(vernacular_name_df, by = all_of(c(vernacular_name_col, group_cols))) %>%

    # Set desired column(s) at the right side
    select(all_of(names(vernacular_name_df)), all_of(out_cols)) %>%

    # Reorder rows based on original order in input df
    right_join(vernacular_name_df, by = names(vernacular_name_df))

  return(out_df)
}
