spatial_mapping_df <- function(data_sf) {
  require("dplyr")
  require("rlang")
  require("sf")


  out_df <- data_sf %>%
    # Remove Amersfoord coordinates
    select(
      -"raw_x_amersfoord",
      -"raw_y_amersfoord"
    ) %>%
    # Keep original X, Y as verbatim coordinates
    rename(
      "dwc_verbatimLatitude" = "raw_y_lambert",
      "dwc_verbatimLongitude" = "raw_x_lambert"
    ) %>%
    # Add CRS
    mutate(
      dwc_verbatimCoordinateSystem = "BD72 / Belgian Lambert 72",
      dwc_verbatimSRS = "EPSG:31370"
    ) %>%
    # Convert to WGS 84 decimal coordinates
    st_transform(4326) %>%
    mutate(
      dwc_decimalLatitude = round(st_coordinates(.data$raw_geometry)[, 2], 5),
      dwc_decimalLongitude = round(st_coordinates(.data$raw_geometry)[, 1], 5),
      dwc_geodeticDatum = "EPSG:4326"
    ) %>%
    st_drop_geometry()

  return(out_df)
}




# Darwin Core mapping function
dwc_mapping <- function(data_sf) {
  require("dplyr")
  require("rlang")
  require("sf")

  # Preparation
  ## Remove columns related to sampling frame
  raw_data <- data_sf %>%
    dplyr::select(
      -"batch",
      -"sample_order"
    )

  ## Add prefix `raw_` to the column names of `raw_data`
  colnames(raw_data) <- paste0("raw_", colnames(raw_data))
  sf::st_geometry(raw_data) <- "raw_geometry"


  # Spatial processing
  spatial_df <- spatial_mapping_df(raw_data)

}
