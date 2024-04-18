# Transform Amersfoort coordinates to Lambert 72 correctly
# see https://inbo.github.io/tutorials/tutorials/spatial_transform_crs_2/
amersfoort_to_lambert72 <- function(sf_object) {
  sf_proj_network(TRUE)

  # Transform CRS according to specific pipeline
  pipelines <- sf_proj_pipelines("EPSG:28992", "EPSG:31370")

  # We select the pipeline with lowest accuracy, by filtering on accuracy
  chosen_pipeline_definition <- pipelines %>%
    slice_min(accuracy, n = 1) %>%
    pull(definition)

  # Transform according to most accurate pipeline
  out_sf <-
    st_transform(st_geometry(sf_object), "EPSG:31370",
                 pipeline = chosen_pipeline_definition
    ) %>%
    st_sf(st_drop_geometry(sf_object), geometry = .) %>%
    as_tibble() %>%
    st_as_sf() %>%
    rename(
      x_amersfoord = x_coord,
      y_amersfoord = y_coord
    ) %>%
    mutate(
      x_lambert = unlist(map(.data$geometry, 1)),
      y_lambert = unlist(map(.data$geometry, 2))
    ) %>%
    select(oid:y_amersfoord, x_lambert, y_lambert, everything())

  return(out_sf)
}

# Function to remove data counted twice at the same spot
process_double_counted_data <- function(counts_df, profs) {

}
