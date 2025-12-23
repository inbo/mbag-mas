process_visits <- function(visits, sample_points) {
  require("dplyr")
  require("lubridate")
  require("rlang")

  out_df <- visits %>%
    # Add sample information
    inner_join(sample_points, by = join_by("pointid")) %>%
    # Add count periods
    mutate(
      date = ymd(paste(.data$jaar, .data$month, .data$day, sep = "-")),
      period_count = case_when(
        date %within% interval(
          ymd(paste(.data$jaar, "04-01", sep = "-")),
          ymd(paste(.data$jaar, "04-20", sep = "-"))
        ) ~ "R1",
        date %within% interval(
          ymd(paste(.data$jaar, "04-21", sep = "-")),
          ymd(paste(.data$jaar, "05-10", sep = "-"))
        ) ~ "R2",
        date %within% interval(
          ymd(paste(.data$jaar, "05-11", sep = "-")),
          ymd(paste(.data$jaar, "06-10", sep = "-"))
        ) ~ "R3",
        date %within% interval(
          ymd(paste(.data$jaar, "06-21", sep = "-")),
          ymd(paste(.data$jaar, "07-15", sep = "-"))
        ) ~ "R4"
      )
    )

  out_df %>%
    # Filter MAS Flanders
    filter(
      .data$jaar >= 2023,
      !(.data$jaar == 2023 & !grepl("leemstreek$", .data$regio)),
      !is.na(.data$period_count)
    ) %>%
    # Clean output
    select(all_of(c(names(visits), "date", "period_count",
                    "regio", "openheid_klasse", "sbp",
                    "x_coord", "y_coord", "crs"))) %>%
    rename("plotnaam" = "pointid", "year" = "jaar") %>%
    arrange(.data$year, .data$plotnaam, .data$date)
}
