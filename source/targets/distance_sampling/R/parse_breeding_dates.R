parse_breeding_dates <- function(df) {
  require("dplyr")
  require("lubridate")
  require("tidyr")
  require("rlang")

  # Tidy dataframe
  trans_vec <- c("jan", "feb", "mrt", "apr", "mei", "jun", "jul", "aug")

  breeding_dates_df <- df %>%
    mutate(across("Datum_begin":"Datum eind_oud", ~ gsub("-", "/", .x))) %>%
    separate(
      .data$Datum_begin,
      into = c("Datum_begin_dag", "Datum_begin_maand"),
      sep = "/"
    ) %>%
    separate(
      .data$Datum_eind,
      into = c("Datum_eind_dag", "Datum_eind_maand"),
      sep = "/"
    ) %>%
    separate(
      .data$`Datum begin_oud`,
      into = c(
        "Datum_begin_dag_oud",
        "Datum_begin_maand_oud"
      ),
      sep = "/"
    ) %>%
    separate(
      .data$`Datum eind_oud`,
      into = c(
        "Datum_eind_dag_oud",
        "Datum_eind_maand_oud"
      ),
      sep = "/"
    ) %>%
    mutate(
      Datum_begin_maand = match(.data$Datum_begin_maand, trans_vec),
      datum_begin = paste(
        .data$Datum_begin_maand,
        .data$Datum_begin_dag,
        sep = "-"
      )
    ) %>%
    mutate(
      Datum_eind_maand = match(.data$Datum_eind_maand, trans_vec),
      datum_eind = paste(
        .data$Datum_eind_maand,
        .data$Datum_eind_dag,
        sep = "-"
      )
    ) %>%
    mutate(
      Datum_begin_maand_oud = match(.data$Datum_begin_maand_oud, trans_vec),
      datum_begin_oud = paste(
        .data$Datum_begin_maand_oud,
        .data$Datum_begin_dag_oud,
        sep = "-"
      )
    ) %>%
    mutate(
      Datum_eind_maand_oud = match(.data$Datum_eind_maand_oud, trans_vec),
      datum_eind_oud = paste(
        .data$Datum_eind_maand_oud,
        .data$Datum_eind_dag_oud,
        sep = "-"
      )
    ) %>%
    mutate(across(
      "datum_begin":"datum_eind_oud",
      ~ ifelse(.x == "NA-NA", NA, .x)
    )) %>%
    select(
      "id" = "Id", "soort" = "Soort", "datum_begin", "datum_eind",
      "datum_begin_oud", "datum_eind_oud", "broedcode",
      "broedcode_oud" = "bc_oud", "wnm_vereist" = "`Waarnemingen verreist`",
      "Opmerking"
    )

  # MAS dates
  r1_start <- "04-01"
  r1_stop <- "04-20"
  r2_start <- "04-21"
  r2_stop <- "05-10"
  r3_start <- "05-11"
  r3_stop <- "06-10"
  r4_start <- "06-21"
  r4_stop <- "07-15"

  # Classify
  suppressWarnings({ # Parsing of NA dates
    out_df <- breeding_dates_df %>%
      select("id", "soort", "datum_begin", "datum_eind") %>%
      mutate(
        jaar = 2024, # does not matter, necessary for parsing dates
        datum_begin2 = ymd(paste(.data$jaar, .data$datum_begin, sep = "-")),
        datum_eind2 = ymd(paste(.data$jaar, .data$datum_eind, sep = "-")),
        R1 = ymd(paste(.data$jaar, r1_start, sep = "-")) %within%
          interval(.data$datum_begin2, .data$datum_eind2) &
          ymd(paste(.data$jaar, r1_stop, sep = "-")) %within%
            interval(.data$datum_begin2, .data$datum_eind2),
        R2 = ymd(paste(.data$jaar, r2_start, sep = "-")) %within%
          interval(.data$datum_begin2, .data$datum_eind2) &
          ymd(paste(.data$jaar, r2_stop, sep = "-")) %within%
            interval(.data$datum_begin2, .data$datum_eind2),
        R3 = ymd(paste(.data$jaar, r3_start, sep = "-")) %within%
          interval(.data$datum_begin2, .data$datum_eind2) &
          ymd(paste(.data$jaar, r3_stop, sep = "-")) %within%
            interval(.data$datum_begin2, .data$datum_eind2),
        R4 = ymd(paste(.data$jaar, r4_start, sep = "-")) %within%
          interval(.data$datum_begin2, .data$datum_eind2) &
          ymd(paste(.data$jaar, r4_stop, sep = "-")) %within%
            interval(.data$datum_begin2, .data$datum_eind2)
      ) %>%
      select("id", "soort", "datum_begin", "datum_eind", "R1", "R2", "R3", "R4")
  })

  out_df
}
