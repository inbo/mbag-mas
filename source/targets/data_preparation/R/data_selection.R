# Select data within periods of time frames
select_within_time_periods <- function(counts_df) {
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
      datum = ymd(paste(jaar, maand, dag, sep = "-")),
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
    filter(!is.na(periode_in_jaar))

  return(out_df)
}

# Select data within circle radius
select_within_radius <- function(counts_df, radius = 300) {
  # Create dataframe with geometry for each count location
  sampling_points <- counts_df %>%
    st_drop_geometry() %>%
    distinct(plotnaam, regio, x_coord, y_coord) %>%
    st_as_sf(coords = c("x_coord", "y_coord"), crs = 31370) %>%
    as_tibble() %>%
    rename(geometry.point = geometry)

  # Calculate distances between observation locations (geometry)
  # and count locations (geometry.point)
  # Select data within radius distance
  obs_to_point_distances <- counts_df %>%
    full_join(sampling_points, by = join_by(plotnaam, regio)) %>%
    mutate(
      calc_distance = st_distance(.$geometry, .$geometry.point,
                                  by_element = TRUE
      ),
      calc_distance = round(units::drop_units(calc_distance))
    ) %>%
    select(-c(distance2plot, geometry.point)) %>%
    rename(distance2plot = calc_distance) %>%
    filter(distance2plot <= radius)

  return(obs_to_point_distances)
}
