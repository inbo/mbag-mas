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

    any(grepl(paste0("^", search_value), vernacular_names))
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
match_vernacular_name <- function(vernacular_name, ...) {
  gbif_lookup <- rgbif::name_lookup(
    vernacular_name,
    datasetKey = "d7dddbf4-2cf0-4f39-9b2a-bb099caae36c",
    ...)

  if (nrow(gbif_lookup$data) > 0) {
    if (nrow(gbif_lookup$data) > 1) {
      taxon_key <- find_df_name(gbif_lookup$names, vernacular_name)
      if (length(taxon_key) == 0) {
        NA_character_
      } else {
        subset(gbif_lookup$data, key == taxon_key)
      }
    } else {
      gbif_lookup$data
    }
  } else {
    NA_character_
  }
}

# Input dataframe with vernacular names and get taxon information
map_taxa_from_vernacular <- function(
    vernacular_name_df,
    vernacular_name_col,
    out_cols = "scientificName",
    ...) {
  require(dplyr)
  require(tidyr)

  # Match vernacular names to get taxonomic info
  matched_names_df <- vernacular_name_df %>%

    # group  by vernacular name and compact the data
    group_by(.data[[vernacular_name_col]]) %>%
    nest() %>%

    # find scientific name for each (distinct) vernacular name
    mutate(taxon_df = purrr::map(
      .data[[vernacular_name_col]],
      match_vernacular_name,
      ...))

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

    # Remove unneeded columns
    select(-"data") %>%

    # Add other columns from input df
    right_join(vernacular_name_df, by = vernacular_name_col) %>%

    # Set desired column(s) at the right side
    select(all_of(names(vernacular_name_df)), all_of(out_cols)) %>%

    # Reorder rows based on original order in input df
    right_join(vernacular_name_df, by = names(vernacular_name_df))

  return(out_df)
}
