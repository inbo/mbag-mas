blur_occurrences <- function(
  occ_df,
  utm_grid_path,
  blur_years = 2,
  embargo_years = 2,
  vulnerable_birds = c("Grauwe Kiekendief", "Bruine Kiekendief",
                       "Patrijs", "Kwartelkoning"),
  vulnerable_mammals = c("Bever", "Das", "Wolf", "Lynx", "Wilde kat")
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
      coordinate_uncertainty = sqrt(2 * (.data$spat_res * 1000) ^ 2) / 2,
      coordinate_uncertainty = as.integer(round(.data$coordinate_uncertainty))
    ) %>%
    select("tag" = "TAG", contains("_centroid"), "spat_res",
           "coordinate_uncertainty", "geometry")


  # Identify occurrences to blur
  # Go back blur_years Octobers
  current_october <- as_date(
    if (month(Sys.Date()) >= 10) {
      paste(year(Sys.Date()), "10", "01", sep = "-")
    } else {
      paste(year(Sys.Date()) - 1, "10", "01", sep = "-")
    }
  )
  blur_date <- current_october %m-% years(blur_years)

  occs_to_blur <- occ_df %>%
    st_as_sf(coords = c("verbatimLongitude", "verbatimLatitude"),
             crs = 31370) %>%
    filter(
      # Blur nesting information for duration of blur_years
      (grepl("nest", .data$verbatimBehavior, ignore.case = TRUE) &
         !(tolower(.data$vernacularName) %in% tolower(vulnerable_birds)) &
         .data$eventDate >= blur_date) |
        # Blur nest information of vulnerable_birds
        (grepl("nest", .data$verbatimBehavior, ignore.case = TRUE) &
           tolower(.data$vernacularName) %in% tolower(vulnerable_birds)) |
        # Blur occurrences of vulnerable_mammals
        (tolower(.data$vernacularName) %in% tolower(vulnerable_mammals))
    ) %>%
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

  # Recalculate event IDs
  occ_blurred_new <- occ_blurred %>%
    # Get original event ID suffices
    rowwise() %>%
    mutate(
      original_suffix = gsub(
        paste0(.data$parentEventID, ":"),
        "",
        .data$eventID,
        fixed = TRUE
      ),
      original_suffix = as.integer(sub("^0+", "", .data$original_suffix))
    ) %>%
    ungroup() %>%
    arrange(.data$eventDate) %>%
    # Regroup by coordinates
    # Blurred records on same centroids are grouped
    group_by(.data$eventDate, .data$locationID) %>%
    mutate(
      coord_key = paste(
        .data$verbatimLatitude,
        .data$verbatimLongitude,
        sep = "_"
      )
    ) %>%
    ungroup() %>%
    # New suffices should start from max. of the original
    group_by(.data$parentEventID) %>%
    mutate(
      max_suffix = max(.data$original_suffix, na.rm = TRUE),

      # identify coordinate groups that need a new suffix
      needs_new = .data$georeferenceRemarks != "",

      # give each *coordinate group* exactly one new number
      new_group_id = if_else(
        .data$needs_new,
        match(.data$coord_key, unique(.data$coord_key[.data$needs_new])),
        NA_integer_
      ),

      event_suffix_num = if_else(
        .data$needs_new,
        .data$max_suffix + .data$new_group_id,
        .data$original_suffix
      )
    ) %>%
    ungroup() %>%
    mutate(
      event_suffix = formatC(.data$event_suffix_num, width = 3, flag = "0"),
      eventID = paste(.data$parentEventID, .data$event_suffix, sep = ":")
    ) %>%
    select(-"coord_key", -"max_suffix", -"needs_new", -"new_group_id",
           -"event_suffix_num", -"event_suffix", -"original_suffix")

  # Remove recent occurrences of vulnerable_birds
  # Go back embargo_years Octobers
  embargo_date <- current_october %m-% years(embargo_years)

  occ_out <- occ_blurred_new %>%
    filter(
      !(grepl("nest", .data$verbatimBehavior, ignore.case = TRUE) &
          tolower(.data$vernacularName) %in% tolower(vulnerable_birds) &
          .data$eventDate >= embargo_date)
    )

  return(occ_out)
}
