#' Summarise distance sampling detection patterns
#'
#' This function generates a textual summary of detection probabilities
#' estimated via distance sampling. It highlights variations across years and
#' strata, and includes the 95% confidence intervals for each estimate.
#'
#' @param df A data frame containing distance sampling results.
#' Required columns:
#'   - `year`: numeric year of observation
#'   - `estimate_p`: estimated detection probability
#'   - `ll_beta`: lower bound of 95% confidence interval
#'   - `ul_beta`: upper bound of 95% confidence interval
#'   - Optional: `regio`, `openheid`, `sbp` (categorical strata)
#' @param threshold Numeric; if range of detection probabilities is below this,
#' considered "consistent" (default 0.05).
#'
#' @return A character string summarising detection probabilities.
#'
#' @export
summarise_detection_patterns <- function(df, threshold = 0.05) { # nolint: cyclocomp_linter
  require("dplyr")
  require("rlang")

  # Helper function to create a label for strata
  # Combines non-empty columns: regio, openheid, sbp
  label_fun <- function(row) {
    parts <- c()
    if (!is.null(row$regio) && !is.na(row$regio) && row$regio != "-") {
      parts <- c(parts, row$regio)
    }
    if (!is.null(row$openheid) && !is.na(row$openheid) && row$openheid != "-") {
      parts <- c(parts, row$openheid)
    }
    if (!is.null(row$sbp) && !is.na(row$sbp) && row$sbp != "-") {
      parts <- c(parts, paste0(row$sbp, " sbp"))
    }
    if (length(parts) == 0) return("")
    paste0("(", paste(parts, collapse = ", "), ")")
  }

  # Start summary text
  txt <- ""

  # Total range of detection probabilities
  range_total <- max(df$estimate_p, na.rm = TRUE) -
    min(df$estimate_p, na.rm = TRUE)

  # If overall variation is low, report consistent detection probabilities
  if (range_total < threshold) {
    txt <- paste0(
      txt,
      "De detectiekansen waren vrij consistent over jaren en strata, ",
      "met waarden rond ",
      sprintf("%.2f [%.2f–%.2f]",
              mean(df$estimate_p, na.rm = TRUE),
              mean(df$ll_beta, na.rm = TRUE),
              mean(df$ul_beta, na.rm = TRUE)),
      "."
    )
    return(txt)
  }

  # Otherwise, describe variation per year
  for (y in sort(unique(df$year))) {
    sub <- df %>% filter(.data$year == y)
    if (nrow(sub) == 0) next

    # Identify highest and lowest detection probabilities
    best <- sub[which.max(sub$estimate_p), ]
    worst <- sub[which.min(sub$estimate_p), ]
    range_y <- max(sub$estimate_p, na.rm = TRUE) -
      min(sub$estimate_p, na.rm = TRUE)

    # If year-specific variation is small, report as consistent
    if (range_y < threshold) {
      year_txt <- paste0(
        "In ", y, " waren de detectiekansen gelijkaardig tussen strata ",
        "(rond ",
        sprintf("%.2f [%.2f–%.2f]",
                mean(sub$estimate_p, na.rm = TRUE),
                mean(sub$ll_beta, na.rm = TRUE),
                mean(sub$ul_beta, na.rm = TRUE)),
        "). "
      )
    } else {
      # Otherwise, report lowest and highest per year with labels
      year_txt <- paste0(
        "In ", y, " varieerden de detectiekansen tussen ",
        sprintf(
          "%.2f [%.2f–%.2f] %s en %.2f [%.2f–%.2f] %s. ",
          worst$estimate_p, worst$ll_beta, worst$ul_beta, label_fun(worst),
          best$estimate_p,  best$ll_beta,  best$ul_beta,  label_fun(best)
        )
      )
    }
    # Append to final summary
    txt <- paste0(txt, year_txt)
  }

  return(txt)
}
