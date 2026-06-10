describe_presence <- function(presence_df) {
  # Get total proportion
  tot_prop <- presence_df %>%
    count(.data$present) %>%
    mutate(prop = .data$n / sum(.data$n)) %>%
    filter(.data$present)
  p <- round(100 * tot_prop$prop)
  txt_total <- paste0(
    "De soort werd in ", p, " % van de bezochte telpunten waargenomen."
  )

  # Summarise presences by landscape openness class and calculate proportions
  openheid <- presence_df %>%
    filter(present) %>%
    count(openheid_klasse) %>%
    mutate(prop = n / sum(n))

  # Generate a sentence describing the species' preference for landscapes
  txt_openheid <- dplyr::case_when(
    any(openheid$openheid_klasse == "OL" & openheid$prop >= 0.6) ~ {
      p <- round(100 * openheid$prop[openheid$openheid_klasse == "OL"])
      paste0(
        "De soort werd vooral waargenomen in open landschap (",
        p,
        " % van de bezochte telpunten)."
      )
    },

    any(openheid$openheid_klasse == "HOL" & openheid$prop >= 0.6) ~ {
      p <- round(100 * openheid$prop[openheid$openheid_klasse == "HOL"])
      paste0(
        "De soort werd vooral waargenomen in halfopen landschap (",
        p,
        " % van de bezochte telpunten)."
      )
    },

    TRUE ~ {
      p_ol <- round(
        100 * sum(openheid$prop[openheid$openheid_klasse == "OL"])
      )
      p_hol <- round(
        100 * sum(openheid$prop[openheid$openheid_klasse == "HOL"])
      )
      paste0(
        "De soort werd ongeveer even vaak in open (",
        p_ol,
        " %) als halfopen landschap (",
        p_hol,
        " %) waargenomen."
      )
    }
  )

  # Summarise presences inside and outside SBP and calculate proportions
  sbp <- presence_df %>%
    filter(present) %>%
    count(sbp) %>%
    mutate(prop = n / sum(n))

  # Generate a sentence describing the species' association with SBP
  txt_sbp <- dplyr::case_when(
    any(sbp$sbp == "binnen" & sbp$prop >= 0.6) ~ {
      p <- round(100 * sbp$prop[sbp$sbp == "binnen"])
      paste0(
        "De soort werd vooral binnen SBP waargenomen (",
        p, " % van de bezochte telpunten)."
      )
    },

    any(sbp$sbp == "buiten" & sbp$prop >= 0.6) ~ {
      p <- round(100 * sbp$prop[sbp$sbp == "buiten"])
      paste0(
        "De soort werd vooral buiten SBP waargenomen (",
        p, " % van de bezochte telpunten)."
      )
    },

    TRUE ~ {
      p_binnen <- round(
        100 * sum(sbp$prop[sbp$sbp == "binnen"])
      )
      p_buiten <- round(
        100 * sum(sbp$prop[sbp$sbp == "buiten"])
      )
      paste0(
        "De soort werd ongeveer even vaak binnen (",
        p_binnen,
        " %) als buiten SBP (",
        p_buiten,
        " %) waargenomen."
      )
    }
  )

  # Calculate the proportion of surveyed locations with a presence per region
  regio <- presence_df %>%
    summarise(
      present = sum(present),
      total = n(),
      .by = regio
    ) %>%
    mutate(prop = present / total)

  # Identify regions where the species occurs in less than 20% of locations
  lage_regios <- regio %>%
    filter(prop < 0.2)

  # Generate a sentence only when one or more regions have very low occurrence
  txt_regio <- if (nrow(lage_regios) == 0) {
    paste("De soort werd in elke regio in meer dan 20 % van de telpunten",
          "waargenomen.")
  } else {
    regio_txt <- paste0(
      lage_regios$regio,
      " (",
      round(100 * lage_regios$prop),
      " %)"
    )

    if (length(regio_txt) == 1) {
      paste0(
        "De soort werd slechts sporadisch waargenomen in de regio ",
        regio_txt,
        "."
      )
    } else {
      paste0(
        "De soort werd slechts sporadisch waargenomen in de regio's ",
        paste(regio_txt, collapse = ", "),
        "."
      )
    }
  }

  # Combine all text fragments into a single paragraph
  paste(
    c(txt_total, txt_openheid, txt_sbp, txt_regio),
    collapse = " "
  )
}
