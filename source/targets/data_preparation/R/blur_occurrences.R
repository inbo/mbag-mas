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
  require("lubridate")

  # Read and prepare UTM grid
  utm_grid_raw <- st_read(utm_grid_path, quiet = TRUE)

  utm_grid <- utm_grid_raw %>%
    st_transform(31370) %>%
    mutate(
      # Get centroid coordinates
      centroids = st_centroid(.data$geometry),
      x_centroid = round(
        st_coordinates(st_transform(.data$centroids, 4326))[, 1], 6
      ),
      y_centroid = round(
        st_coordinates(st_transform(.data$centroids, 4326))[, 2], 6
      ),
      x_centroid_lambert = round(
        st_coordinates(.data$centroids)[, 1], 1
      ),
      y_centroid_lambert = round(
        st_coordinates(.data$centroids)[, 2], 1
      ),
      # Calculate uncertainty
      spat_res = case_when(
        nchar(.data$TAG) == 6 ~ 1,
        nchar(.data$TAG) == 5 ~ 5,
        nchar(.data$TAG) == 4 ~ 10,
        TRUE ~ NA_real_
      ),
      coordinate_uncertainty = sqrt(2 * (.data$spat_res * 1000) ^ 2) / 2
    ) %>%
    select("tag" = "TAG", contains("_centroid"), "spat_res",
           "coordinate_uncertainty", "geometry")


  # Identify occurrences to blur
  occs_to_blur <- occ_df %>%
    st_as_sf(coords = c("verbatimLongitude", "verbatimLatitude"),
             crs = 31370) %>%
    filter(grepl("nest", .data$verbatimBehavior, ignore.case = TRUE) |
             tolower(.data$vernacularName) %in% tolower(embargo_species)) %>%
    st_join(utm_grid, join = st_within, left = FALSE) %>%
    st_drop_geometry() %>%
    select(
      "occurrenceID",
      contains("_centroid"),
      "coordinate_uncertainty",
      "spat_res"
    )

  # Merge blur information into main dataset
  occ_blurred <- occ_df %>%
    left_join(occs_to_blur, by = "occurrenceID") %>%
    mutate(
      is_blurred = !is.na(.data$spat_res),
      # Change coordinate columns
      decimalLatitude = if_else(
        .data$is_blurred,
        .data$y_centroid,
        .data$decimalLatitude
      ),
      decimalLongitude = if_else(
        .data$is_blurred,
        .data$x_centroid,
        .data$decimalLongitude
      ),
      verbatimLatitude = if_else(
        .data$is_blurred,
        .data$y_centroid_lambert,
        .data$verbatimLatitude
      ),
      verbatimLongitude = if_else(
        .data$is_blurred,
        .data$x_centroid_lambert,
        .data$verbatimLongitude
      ),
      coordinateUncertaintyInMeters = if_else(
        .data$is_blurred,
        .data$coordinate_uncertainty,
        .data$coordinateUncertaintyInMeters
      ),
      # Create new columns
      informationWithheld = if_else(
        .data$is_blurred,
        "original locations available upon request",
        ""
      ),
      dataGeneralizations = if_else(
        .data$is_blurred,
        paste0("UTM ", .data$spat_res, " km"),
        ""
      ),
      georeferenceRemarks = if_else(
        .data$is_blurred,
        "coordinates are centroid of used grid square",
        ""
      )
    ) %>%
    select(-contains("_centroid"), -"coordinate_uncertainty", -"spat_res",
           -"is_blurred")

  # Remove recent occurrences of vulnerable species
  embargo_year <- year(Sys.time()) - 2
  embargo_date <- as_date(paste(embargo_year, 1, 1, "-"))

  occ_out <- occ_blurred %>%
    filter(
      !(tolower(.data$vernacularName) %in% tolower(embargo_species) &
          .data$eventDate < embargo_date)
    )

  return(occ_out)
}
