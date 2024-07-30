# Function to determine the structure of the data frames in the list
get_df_structure <- function(df_list) {
  non_na_df <- df_list %>% compact() %>% first()
  empty_df <- map(non_na_df, ~ NA)
  empty_df <- as_tibble(empty_df)
  return(empty_df)
}

# https://gist.github.com/damianooldoni/3fa9cc1ffa67377a9757df097d48d19f
# input: a vernacular name
# output: the best matched scientific name from the GBIF Backbone
# NA_character_ if no match found
match_vernacular_name <- function(vernacular_name, ...) {
  names <- rgbif::name_lookup(
    vernacular_name,
    datasetKey = "d7dddbf4-2cf0-4f39-9b2a-bb099caae36c",
    limit = 1, # this returns the most likely taxon
    ...)$data

  if (nrow(names) > 0) {
    names
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

  # Match vernacular names to get taxonomic info
  matched_names_df <- vernacular_name_df %>%

    # group  by vernacular name and compact the data
    group_by(.data[[vernacular_name_col]]) %>%
    nest() %>%

    # find scientific name for each (distinct) vernacular name
    mutate(taxon_df = map(.data[[vernacular_name_col]],
                          match_vernacular_name,
                          ...))

  # Create output dataframe
  out_df <- matched_names_df %>%

    # Unnest taxon dataframes
    mutate(taxon_df = map(.data$taxon_df, ~ {
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
