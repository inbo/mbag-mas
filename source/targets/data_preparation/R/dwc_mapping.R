dwc_mapping <- function(data_sf) {
  require(dplyr)
  require(rlang)
  require(sf)

  # Spatial processing
  ## Lambert to Decimals: keep original X, Y as verbatim coordinates
  taxon_core <- data_sf %>%
    select(-"x_amersfoord", -"y_amersfoord") %>%
    rename("verbatimLatitude" = "y_lambert",
           "verbatimLongitude" = "x_lambert") %>%
    mutate(
      verbatimCoordinateSystem = "BD72 / Belgian Lambert 72",
      verbatimSRS = "EPSG:31370") %>%
    st_transform(4326) %>%
    mutate(
      decimalLatitude = st_coordinates(.data$geometry)[, 2],
      decimalLongitude = st_coordinates(.data$geometry)[, 1],
      geodeticDatum = "EPSG:4326") %>%
    st_drop_geometry()

  # Static values


}
