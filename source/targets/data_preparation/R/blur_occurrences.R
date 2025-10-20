blur_occurrences <- function(
  occ_df,
  utm_grid_path,
  blur_time = 2,
  embargo_time = 2,
  embargo_species = c("Grauwe Kiekendief", "Bruine Kiekendief",
                      "Patrijs", "Kwartelkoning")
) {
  require("sf")
  require("dplyr")
  require("rlang")

  # Read shape file
  utm_grid_raw <- st_read(utm_grid_path, quiet = TRUE)

  utm_grid <- utm_grid_raw %>%
    st_transform(31370) %>%
    mutate(
      # Get centroid coordinates
      centroids = st_centroid(.data$geometry),
      x_centroid = st_coordinates(st_transform(.data$centroids, 4326))[, 1],
      y_centroid = st_coordinates(st_transform(.data$centroids, 4326))[, 2],
      x_centroid_lambert = st_coordinates(.data$centroids)[, 1],
      y_centroid_lambert = st_coordinates(.data$centroids)[, 2],
      # Calculate uncertainty
      spat_res = case_when(
        nchar(.data$TAG) == 6 ~ 1,
        nchar(.data$TAG) == 5 ~ 5,
        nchar(.data$TAG) == 4 ~ 10,
      ),
      coordinate_uncertainty = sqrt(2 * (.data$spat_res * 1000) ^ 2) / 2
    ) %>%
    select("tag" = "TAG", contains("_centroid"), "spat_res",
           "coordinate_uncertainty", "geometry")



}
