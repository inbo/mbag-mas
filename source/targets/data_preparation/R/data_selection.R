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
