get_presences <- function(df) {
  df %>%
    group_by(
      plotnaam, x_plot, y_plot, periode_in_jaar, regio, openheid_klasse, sbp,
      stratum, jaar
    ) %>%
    summarise(count = sum(aantal), .groups = "drop") %>%
    select(
      count, plotnaam, periode_in_jaar, regio, openheid_klasse, sbp,
      stratum, x_plot, y_plot, jaar
    )
}

get_absences <- function(presences, design) {
  design %>%
    select(
      "plotnaam" = "pointid", "regio", "openheid_klasse", "sbp",
      "x_plot" = "x_coord", "y_plot" = "y_coord"
    ) %>%
    expand_grid(periode_in_jaar = paste0("R", 1:4)) %>%
    anti_join(
      presences,
      by = join_by(
        plotnaam, regio, openheid_klasse, sbp, x_plot, y_plot,
        periode_in_jaar
      )
    ) %>%
    mutate(
      count = 0,
      stratum = ifelse(
        regio == "Weidestreek",
        "Weidestreek",
        paste(regio, openheid_klasse, sbp, sep = " - ")
      ),
      jaar = unique(presences$jaar),
      tar_group = unique(presences$tar_group)
    ) %>%
    select(
      count, plotnaam, periode_in_jaar, regio, openheid_klasse, sbp,
      stratum, x_plot, y_plot, jaar, tar_group
    )
}
