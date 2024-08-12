# Select data within sampling
join_with_sample <- function(counts_df, sample) {
  # Join with sample
  sample_counts <- dplyr::inner_join(
      x = counts_df,
      y = sample,
      by =  dplyr::join_by("plotnaam" == "pointid")
    )


}

# Select data within periods of time frames
select_within_time_periods <- function(counts_df) {
  require("dplyr")
  require("rlang")
  require("lubridate")

  # Define valid periods
  r1_start <- "04-01"
  r1_stop <- "04-20"
  r2_start <- "04-21"
  r2_stop <- "05-10"
  r3_start <- "05-11"
  r3_stop <- "06-10"
  r4_start <- "06-21"
  r4_stop <- "07-15"

  # Select count data within time periods
  out_df <- counts_df %>%
    mutate(
      datum = ymd(paste(.data$jaar, .data$maand, .data$dag, sep = "-")),
      periode_in_jaar = case_when(
        datum %within% interval(
          ymd(paste(jaar, r1_start, sep = "-")),
          ymd(paste(jaar, r1_stop, sep = "-"))
        ) ~ "R1",
        datum %within% interval(
          ymd(paste(jaar, r2_start, sep = "-")),
          ymd(paste(jaar, r2_stop, sep = "-"))
        ) ~ "R2",
        datum %within% interval(
          ymd(paste(jaar, r3_start, sep = "-")),
          ymd(paste(jaar, r3_stop, sep = "-"))
        ) ~ "R3",
        datum %within% interval(
          ymd(paste(jaar, r4_start, sep = "-")),
          ymd(paste(jaar, r4_stop, sep = "-"))
        ) ~ "R4"
      )
    ) %>%
    filter(!is.na(.data$periode_in_jaar))

  return(out_df)
}

# Calculate distance of observation to center point
calculate_obs_dist <- function(counts_df) {
  require("dplyr")
  require("rlang")
  require("sf")

  # Create dataframe with geometry for each count location
  sampling_points <- counts_df %>%
    st_drop_geometry() %>%
    distinct(.data$plotnaam, .data$regio, .data$x_coord, .data$y_coord) %>%
    st_as_sf(coords = c("x_coord", "y_coord"), crs = 31370) %>%
    as_tibble() %>%
    rename(geometry.point = "geometry")

  # Calculate distances between observation locations (geometry)
  # and count locations (geometry.point)
  obs_to_point_distances <- counts_df %>%
    full_join(sampling_points, by = join_by("plotnaam", "regio")) %>%
    mutate(
      calc_distance = st_distance(.data$geometry, .data$geometry.point,
                                  by_element = TRUE
      ),
      calc_distance = round(units::drop_units(.data$calc_distance))
    ) %>%
    select(-c("distance2plot", "geometry.point")) %>%
    rename(distance2plot = "calc_distance")

  return(obs_to_point_distances)
}
