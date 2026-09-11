add_absences <- function(presence_data, visits) {
  require("dplyr")

  species <- unique(presence_data$naam)

  visits %>%
    select(
      "plotid",
      "plotnaam",
      "year",
      "regio",
      "openheid_klasse",
      "sbp",
      "period_count"
    ) %>%
    distinct() %>%
    left_join(
      presence_data,
      by = c(
        "plotid",
        "plotnaam",
        "year" = "jaar",
        "regio",
        "openheid_klasse",
        "sbp",
        "period_count" = "periode_in_jaar"
      )
    ) %>%
    mutate(
      naam = coalesce(.data$naam, species),
      count = coalesce(.data$count, 0)
    )
}
