get_presences <- function(df) {
  require("dplyr")
  df %>%
    group_by(
      .data$plotnaam, .data$x_plot, .data$y_plot, .data$periode_in_jaar,
      .data$regio, .data$openheid_klasse, .data$sbp, .data$stratum, .data$jaar
    ) %>%
    summarise(count = sum(.data$aantal), .groups = "drop") %>%
    select(
      "count", "plotnaam", "periode_in_jaar", "regio", "openheid_klasse", "sbp",
      "stratum", "x_plot", "y_plot", "jaar"
    )
}

get_absences <- function(presences, design) {
  require("dplyr")
  require("rlang")

  design %>%
    select(
      "plotnaam" = "pointid", "regio", "openheid_klasse", "sbp",
      "x_plot" = "x_coord", "y_plot" = "y_coord"
    ) %>%
    tidyr::expand_grid(periode_in_jaar = paste0("R", 1:4)) %>%
    anti_join(
      presences,
      by = join_by(
        "plotnaam", "regio", "openheid_klasse", "sbp", "x_plot", "y_plot",
        "periode_in_jaar"
      )
    ) %>%
    mutate(
      count = 0,
      stratum = ifelse(
        .data$regio == "Weidestreek",
        "Weidestreek",
        paste(.data$regio, .data$openheid_klasse, .data$sbp, sep = " - ")
      ),
      jaar = unique(presences$jaar),
      tar_group = unique(presences$tar_group)
    ) %>%
    select(
      "count", "plotnaam", "periode_in_jaar", "regio", "openheid_klasse", "sbp",
      "stratum", "x_plot", "y_plot", "jaar", "tar_group"
    )
}
