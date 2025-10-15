# Function to describe trends within a plot
describe_trend_table <- function(
  df,
  all_trends = c("altijd bedekt", "altijd onbedekt",
                 "toename bedekking", "afname bedekking",
                 "parabool", "omgekeerde parabool",
                 "complex patroon")
) {
  require("glue")
  require("dplyr")

  df %>%
    mutate(y = year) %>%
    group_by(year) %>%
    group_map(~{
      data_year <- .x

      total <- sum(data_year$n)
      avg_complex <- weighted.mean(data_year$complexity, data_year$n)
      complexity_level <- ifelse(avg_complex > 1.5, "meer complexe",
                                 "meer eenvoudige")

      most_common <- data_year %>%
        filter(n == max(n))
      most_common_vec <- paste(most_common$trend, collapse = ", ")
      least_common <- data_year %>%
        filter(n == min(n))
      least_common_vec <- paste(least_common$trend, collapse = ", ")
      missing <- setdiff(all_trends, data_year$trend)
      missing_vec <- paste(missing, collapse = ", ")

      if (nrow(most_common) > 1) {
        most_common_trend <- "trends"
        most_common_voorkomen <- "voorkomen"
        most_common_is <- "zijn"
      } else {
        most_common_trend <- "trend"
        most_common_voorkomen <- "voorkomt"
        most_common_is <- "is"
      }

      if (nrow(least_common) > 1) {
        least_common_voorkomen <- "voorkomen"
      } else {
        least_common_voorkomen <- "voorkomt"
      }

      if (length(missing) > 1) {
        missing_trends <- "trends"
        missing_komen <- "komen"
      } else {
        missing_trends <- "trend"
        missing_komen <- "komt"
      }

      text <- glue(
        "In {unique(data_year$y)} zien we vooral {complexity_level} trends. ",
        paste(
          "De {most_common_trend} die het vaakst {most_common_voorkomen}",
          "{most_common_is} {most_common_vec} (n = {unique(most_common$n)}),",
          "terwijl {least_common_vec} het minst {least_common_voorkomen}",
          "(n = {unique(least_common$n)}). "
        ),
        if (length(missing) > 0) {
          glue(
            paste("De volgende {missing_trends} {missing_komen} in dit jaar",
                  "niet voor: {missing_vec}.")
          )
        } else {
          "Alle mogelijke trendcategorieën komen in dit jaar voor."
        }
      )

      text
    }) %>%
    unlist() %>%
    paste(collapse = " ")
}


describe_summary_tables <- function(df, var, tol = 0.05) {
  require("dplyr")
  require("rlang")
  require("glue")

  summarise_bare_soil <- function(df, var) {
    df %>%
      group_by(.data[[var]]) %>%
      summarise(
        min_bs = min(.data$perc_bare_soil, na.rm = TRUE),
        q25_bs = quantile(.data$perc_bare_soil, 0.25, na.rm = TRUE),
        median_bs = median(.data$perc_bare_soil, na.rm = TRUE),
        q75_bs = quantile(.data$perc_bare_soil, 0.75, na.rm = TRUE),
        max_bs = max(.data$perc_bare_soil, na.rm = TRUE),
        mean_bs = mean(.data$perc_bare_soil, na.rm = TRUE),
        sd_bs = sd(.data$perc_bare_soil, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(.data$mean_bs))
  }

  # Compute summaries for both indicators
  ndvi_summary <- summarise_bare_soil(df$ndvi, var)
  bsi_summary  <- summarise_bare_soil(df$bsi, var)

  # Function to check if orders are effectively the same within tolerance
  same_order_tol <- function(ndvi_df, bsi_df, tol) {
    if (identical(ndvi_df[[var]], bsi_df[[var]])) {
      return(TRUE)
    } else {
      if (nrow(ndvi_df) == 2) {
        bool <- abs(ndvi_df$mean_bs[1] - ndvi_df$mean_bs[2]) < tol &&
          abs(bsi_df$mean_bs[1] - bsi_df$mean_bs[2])
      } else {
        bool <- FALSE
      }
      return(bool)
    }
  }

  same_order <- same_order_tol(ndvi_summary, bsi_summary, tol)

  # Helper: natural Dutch text summary (like your original version)
  describe_one <- function(summary_df, indicator_label) {
    var_text <- apply(summary_df, 1, function(row) {
      sprintf("%s (gemiddelde: %.3f, mediaan: %.3f, SD: %.3f)",
              row[[var]], as.numeric(row[["mean_bs"]]),
              as.numeric(row[["median_bs"]]), as.numeric(row[["sd_bs"]]))
    })

    if (nrow(summary_df) > 2) {
      sprintf(
        paste(
          "Het hoogste percentage naakte bodem, gemiddeld over alle jaren",
          "(%s), is in %s, gevolgd door %s."
        ),
        indicator_label,
        var_text[1],
        paste(var_text[-1], collapse = ", ")
      )
    } else if (nrow(summary_df) == 2) {
      if (abs(summary_df$mean_bs[1] - summary_df$mean_bs[2]) < tol) {
        sprintf(
          paste("Het percentage naakte bodem, gemiddeld over alle jaren,",
                "is gelijkaardig tussen %s en %s (%s)."),
          var_text[1], var_text[2], indicator_label
        )
      } else {
        sprintf(
          paste("Het percentage naakte bodem, gemiddeld over alle jaren,",
                "is hoger in %s dan in %s (%s)."),
          var_text[1], var_text[2], indicator_label
        )
      }
    } else {
      stop("At least two categories needed.")
    }
  }

  # Output depends on similarity
  if (same_order) {
    # Combined description
    var_text <- mapply(
      function(nrow, brow) {
        sprintf(
          paste("%s (NDVI – gemiddelde: %.3f, mediaan: %.3f, SD: %.3f;",
                "BSI – %.3f, %.3f, %.3f)"),
          nrow[[var]],
          nrow[["mean_bs"]], nrow[["median_bs"]], nrow[["sd_bs"]],
          brow[["mean_bs"]], brow[["median_bs"]], brow[["sd_bs"]]
        )
      },
      split(ndvi_summary, seq_len(nrow(ndvi_summary))),
      split(bsi_summary, seq_len(nrow(bsi_summary)))
    )

    if (nrow(ndvi_summary) > 2) {
      out_text <- sprintf(
        paste("De resultaten zijn vergelijkbaar tussen NDVI en BSI.",
              "Het hoogste percentage naakte bodem, gemiddeld over alle jaren,",
              "is in %s, gevolgd door %s."),
        var_text[1],
        paste(var_text[-1], collapse = ", ")
      )
    } else {
      if (abs(ndvi_summary$mean_bs[1] - ndvi_summary$mean_bs[2]) < tol &&
            abs(bsi_summary$mean_bs[1] - bsi_summary$mean_bs[2]) < tol) {
        out_text <- sprintf(
          paste("De resultaten zijn vergelijkbaar tussen NDVI en BSI.",
                "Het percentage naakte bodem, gemiddeld over alle jaren,",
                "is gelijkaardig tussen %s en %s."),
          var_text[1], var_text[2]
        )
      } else {
        text <- sprintf(
          paste("Het percentage naakte bodem, gemiddeld over alle jaren,",
                "is hoger %s, dan %s."),
          var_text[1],
          paste(var_text[-1], collapse = ", ")
        )
        out_text <- glue(
          "De resultaten zijn vergelijkbaar tussen NDVI en BSI. ",
          "{text}"
        )
      }
    }

  } else {
    # Distinct orders → separate narratives
    ndvi_text <- describe_one(ndvi_summary, "NDVI")
    bsi_text  <- describe_one(bsi_summary, "BSI")

    out_text <- glue(
      "De volgorde verschilt tussen de indicatoren. ",
      "{ndvi_text} {bsi_text}"
    )
  }

  return(out_text)
}
