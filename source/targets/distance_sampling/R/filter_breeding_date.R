filter_breeding_date <- function(df, dates, exception) {
  require("dplyr")
  require("rlang")

  spec <- unique(df$naam)
  dates <- tidyr::drop_na(dates)

  if ((spec %in% dates$soort) && !(spec %in% exception)) {
    # Select breeding dates
    count_periods_df <- dates %>%
      filter(.data$soort == spec) %>%
      select("R1":"R4")
    count_periods_df <- unlist(count_periods_df[1, ])
    count_periods <- names(count_periods_df[which(count_periods_df == TRUE)])
  } else {
    count_periods <- c("R1", "R2", "R3", "R4")
  }

  df %>%
    filter(.data$periode_in_jaar %in% count_periods)
}
