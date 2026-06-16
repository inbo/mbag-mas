detectable_effect_to_df <- function(x) {
  require("dplyr")
  require("rlang")

  res <- lapply(
    names(x),
    function(target_name) {
      value <- x[[target_name]]

      data.frame(
        beta3_low = value[2],
        beta3 = value[1],
        beta3_high = value[3],
        as.data.frame(attr(value, "design")),
        check.names = FALSE
      ) %>%
        mutate(
          effect_pct = 100 * (exp(.data$beta3) - 1),
          effect_pct_low = 100 * (exp(.data$beta3_low) - 1),
          effect_pct_high = 100 * (exp(.data$beta3_high) - 1),
          effect_pct_10 = 100 * (exp(.data$beta3 * 10) - 1),
          effect_pct_10_low = 100 * (exp(.data$beta3_low * 10) - 1),
          effect_pct_10_high = 100 * (exp(.data$beta3_high * 10) - 1)
        )
    }
  )

  dplyr::bind_rows(res)
}
