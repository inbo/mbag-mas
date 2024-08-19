# Select data within sampling
join_with_sample <- function(counts_df, sample) {
  require("dplyr")
  require("rlang")

  # Join with sample
  sample_counts <- inner_join(
      x = counts_df,
      y = sample,
      by =  join_by("plotnaam" == "pointid")
    )

  # Select data from correct years
  out_df <- sample_counts %>%
    mutate(keep = case_when(
      .data$regio == "Oostelijke leemstreek" & .data$jaar >= 2018 ~ TRUE,
      (.data$regio == "Westelijke leemstreek" |
          .data$regio == "Zandleemstreek") & .data$jaar >= 2023 ~ TRUE,
      (.data$regio == "Polders" |
         .data$regio == "Kempen" |
         .data$regio == "Zandstreek" |
         .data$regio == "Weidestreek") & .data$jaar >= 2024 ~ TRUE,
      .default = FALSE
      )
    ) %>%
    filter(.data$keep) %>%
    select(-"keep")

  return(out_df)
}

# Expand sample with years based on dataframe
expand_sample_by_year <- function(sample_df, data_df, year_var) {
  require("dplyr")
  require("rlang")
  year_range <- sort(pull(distinct(st_drop_geometry(data_df[year_var]))))

  out_df <- sample_df %>%
    tidyr::expand_grid(jaar = year_range) %>%
    mutate(keep = case_when(
      .data$regio == "Oostelijke leemstreek" & .data$jaar >= 2018 ~ TRUE,
      (.data$regio == "Westelijke leemstreek" |
         .data$regio == "Zandleemstreek") & .data$jaar >= 2023 ~ TRUE,
      (.data$regio == "Polders" |
         .data$regio == "Kempen" |
         .data$regio == "Zandstreek" |
         .data$regio == "Weidestreek") & .data$jaar >= 2024 ~ TRUE,
      .default = FALSE
    )
    ) %>%
    filter(.data$keep) %>%
    select(-"keep")

  return(out_df)
}

# Select data within periods of time frames
select_within_time_periods <- function(counts_df) {
  require("dplyr")
  require("rlang")
  require("lubridate")

  # Select count data within time periods
  out_df <- counts_df %>%
    mutate(
      datum = ymd(paste(.data$jaar, .data$maand, .data$dag, sep = "-")),
      periode_in_jaar = case_when(
        datum %within% interval(
          ymd(paste(jaar, "04-01", sep = "-")),
          ymd(paste(jaar, "04-20", sep = "-"))
        ) ~ "R1",
        datum %within% interval(
          ymd(paste(jaar, "04-21", sep = "-")),
          ymd(paste(jaar, "05-10", sep = "-"))
        ) ~ "R2",
        datum %within% interval(
          ymd(paste(jaar, "05-11", sep = "-")),
          ymd(paste(jaar, "06-10", sep = "-"))
        ) ~ "R3",
        datum %within% interval(
          ymd(paste(jaar, "06-21", sep = "-")),
          ymd(paste(jaar, "07-15", sep = "-"))
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
