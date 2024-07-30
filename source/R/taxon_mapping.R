# Function to determine the structure of the data frames in the list
get_structure <- function(df_list) {
  non_na_df <- df_list %>% compact() %>% .[[1]]
  empty_df <- map(non_na_df, ~ NA)
  empty_df <- as_tibble(empty_df)
  return(empty_df)
}

# https://gist.github.com/damianooldoni/3fa9cc1ffa67377a9757df097d48d19f
# define get_vernacular_name() core function
# input: a vernacular name
# output: the best matched scientific name from the GBIF Backbone, NA_character_ if no match found
get_vernacular_name <- function(vernacular_name) {
  names <- rgbif::name_lookup(
    vernacular_name,
    datasetKey = "d7dddbf4-2cf0-4f39-9b2a-bb099caae36c",
    limit = 1)$data # this returns the most likely taxon

  if (nrow(names) > 0) {
    names
  } else {
    NA_character_
  }
}

map_taxa_from_vernacular <- function(
    vernacular_name_df,
    vernacular_name_col = "dwc_vernacularName") {

  n_vernacular_names_df <- vernacular_names_df %>%

    # group  by vernacular name and compact the data
    group_by(vernacular_name) %>%
    nest() %>%

    # find scientific name for each (distinct) vernacular name
    mutate(scientificName = map(vernacular_name, get_vernacular_name))


  # Determine the structure of the data frames
  empty_df <- get_structure(n_vernacular_names_df$scientificName)

  out_df <- n_vernacular_names_df %>%
    mutate(scientificName = map(scientificName, ~
                                  if(all(is.na(.))) empty_df else .)) %>%
    unnest(scientificName) %>%
    # ungroup result
    ungroup() %>%
    # remove unneeded columns
    select(-one_of("data")) %>%

    # add other columns from input df vernacular_names_df
    right_join(vernacular_names_df, by = "vernacular_name") %>%

    # set new column scientificName at the right side
    select(all_of(names(vernacular_names_df)), scientificName) %>%

    # reorder rows based on original order in input df
    right_join(vernacular_names_df,
               by = names(vernacular_names_df))

  return(out_df)
}
