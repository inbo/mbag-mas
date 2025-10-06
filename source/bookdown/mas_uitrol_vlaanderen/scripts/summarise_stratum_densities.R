#' Summarise distance sampling density patterns per stratum
#'
#' Generates a narrative description of density estimates across regions and
#' years, highlighting general patterns (OL vs HOL, binnen vs buiten SBP),
#' giving year-specific details only if patterns differ between years,
#' and comparing results to policy thresholds if provided.
#' Additionally, reports the highest and lowest density per stratum each year
#' (lowest only if > 0).
#'
#' @param data A data frame with the following required columns:
#'   \describe{
#'     \item{regio}{Region (stratum name).}
#'     \item{openheid}{Factor with values \code{"OL"} or \code{"HOL"}.}
#'     \item{sbp}{Factor with values \code{"binnen"} or \code{"buiten"}
#'           (inside/outside SBP).}
#'     \item{estimate}{Estimated density (numeric).}
#'     \item{lcl}{Lower confidence limit (numeric).}
#'     \item{ucl}{Upper confidence limit (numeric).}
#'     \item{species}{Species name (character).}
#'     \item{year}{Year of the estimate (numeric or factor).}
#'   }
#'   Optionally, if threshold comparisons are available:
#'   \describe{
#'     \item{effect}{Categorical: \code{"boven"}, \code{"onder"}, or
#'           \code{"geen sign.\nverschil"}.}
#'     \item{threshold}{Numeric streefwaarde (policy target).}
#'   }
#'
#' @param threshold Numeric ratio used to define a "remarkable" difference
#'   (default = 1.5, i.e. ≥50% higher).
#'
#' @return A single character string with a continuous text description.
#' @export
#'
#' @examples
#' # Example with mock data
#' df <- data.frame(
#'   regio = rep(c("Kempen", "Leemstreek"), each = 4),
#'   openheid = rep(c("OL","HOL"), 4),
#'   sbp = rep(c("binnen","buiten"), 4),
#'   estimate = runif(8, 0, 3),
#'   lcl = runif(8, 0, 2),
#'   ucl = runif(8, 2, 4),
#'   species = "Geelgors",
#'   year = rep(2024, 8)
#' )
#' summarise_stratum_densities(df)
summarise_stratum_densities <- function(data, threshold = 1.5) { # nolint: cyclocomp_linter
  # Check that required columns are present
  stopifnot(
    all(
      c("regio", "openheid", "sbp", "estimate", "lcl", "species", "year") %in%
        names(data)
    )
  )

  years <- sort(unique(data$year))    # all years present
  regions <- sort(unique(data$regio)) # all regions present

  # Identify regions that are absent in all years
  absent_regions <- regions[sapply(regions, function(reg) {
    reg_rows <- data[data$regio == reg, ]
    all(reg_rows$estimate == 0 | reg_rows$lcl < 0.1)
  })]

  # Function to summarise OL vs HOL and binnen vs buiten per region for a given
  # year
  summarize_year <- function(subdata) {
    ol_vs_hol <- bin_vs_bui <- setNames(vector("list", length(regions)),
                                        regions)
    for (reg in regions) {
      df <- subdata[subdata$regio == reg, ]

      # Skip regions absent across all years
      if (reg %in% absent_regions) {
        ol_vs_hol[[reg]] <- bin_vs_bui[[reg]] <- "absent"
        next
      }

      # Mark strata where species is practically absent
      df$present <- !(df$estimate == 0 | df$lcl < 0.1)
      if (all(!df$present)) {
        ol_vs_hol[[reg]] <- bin_vs_bui[[reg]] <- "absent"
        next
      }

      # Compare OL vs HOL using sum of densities
      sum_ol <- sum(df$estimate[df$openheid == "OL" & df$present],
                    na.rm = TRUE)
      sum_hol <- sum(df$estimate[df$openheid == "HOL" & df$present],
                     na.rm = TRUE)
      ol_vs_hol[[reg]] <- if (sum_ol > threshold * sum_hol) {
        "OL"
      } else if (sum_hol > threshold * sum_ol) {
        "HOL"
      } else {
        "similar"
      }

      # Compare binnen vs buiten SBP using sum of densities
      sum_bin <- sum(df$estimate[df$sbp == "binnen" & df$present],
                     na.rm = TRUE)
      sum_bui <- sum(df$estimate[df$sbp == "buiten" & df$present],
                     na.rm = TRUE)
      bin_vs_bui[[reg]] <- if (sum_bin > threshold * sum_bui) {
        "binnen"
      } else if (sum_bui > threshold * sum_bin) {
        "buiten"
      } else {
        "similar"
      }
    }
    list(ol_vs_hol = ol_vs_hol, bin_vs_bui = bin_vs_bui)
  }

  # Compute OL/HOL and SBP patterns per year
  patterns_by_year <- lapply(years, function(y) {
    summarize_year(data[data$year == y, ])
  })
  names(patterns_by_year) <- years

  # Check if OL/HOL and SBP patterns are consistent across years
  ol_consistent <- all(
    sapply(patterns_by_year[-1], function(p) {
      identical(p$ol_vs_hol, patterns_by_year[[1]]$ol_vs_hol)
    })
  )
  bin_consistent <- all(
    sapply(patterns_by_year[-1], function(p) {
      identical(p$bin_vs_bui, patterns_by_year[[1]]$bin_vs_bui)
    })
  )

  text_parts <- list()

  # Absence statement for regions absent in all years
  if (length(absent_regions) > 0) {
    text_parts <- c(text_parts,
                    sprintf("De soort is praktisch afwezig in %s.",
                            paste(absent_regions, collapse = ", ")))
  }

  # Function to create narrative for a region in a year
  describe_region <- function(region, ol, bin) {
    if (ol == "absent" && bin == "absent") return(NULL)
    desc_ol <- if (ol == "OL") {
      "OL was hoger dan HOL"
    } else if (ol == "HOL") {
      "HOL was hoger dan OL"
    } else {
      "OL en HOL waren vergelijkbaar"
    }
    desc_bin <- if (bin == "binnen") {
      "binnen SBP lag hoger dan buiten SBP"
    } else if (bin == "buiten") {
      "buiten SBP lag hoger dan binnen SBP"
    } else {
      "binnen en buiten SBP waren vergelijkbaar"
    }
    sprintf("In %s: %s, en %s.", region, desc_ol, desc_bin)
  }

  # Add regional patterns: one block if consistent, otherwise per year
  if (ol_consistent && bin_consistent) {
    combined <- patterns_by_year[[1]]
    regional_text <- mapply(describe_region, names(combined$ol_vs_hol),
                            combined$ol_vs_hol, combined$bin_vs_bui,
                            SIMPLIFY = TRUE)
    text_parts <- c(text_parts, regional_text[!sapply(regional_text, is.null)])
  } else {
    for (y in years) {
      text_parts <- c(text_parts, sprintf("Jaar %s:", y))
      combined <- patterns_by_year[[as.character(y)]]
      regional_text <- mapply(describe_region, names(combined$ol_vs_hol),
                              combined$ol_vs_hol, combined$bin_vs_bui,
                              SIMPLIFY = TRUE)
      text_parts <- c(text_parts, regional_text[!sapply(regional_text,
                                                        is.null)])
    }
  }

  # Report highest and lowest density per year (skip lowest if 0)
  for (y in years) {
    df_year <- data[data$year == y, ]
    if (nrow(df_year) == 0) next

    # Highest density
    max_row <- df_year[which.max(df_year$estimate), ]
    highest_text <- sprintf(
      paste(
        "In %s werd de hoogste dichtheid gevonden in %s in %s, %s SBP",
        "(%.2f [%.2f–%.2f] broedparen/100 ha)."
      ),
      y, max_row$regio, max_row$openheid, max_row$sbp,
      max_row$estimate, max_row$lcl, max_row$ucl
    )
    text_parts <- c(text_parts, highest_text)

    # Lowest density (if > 0)
    min_row <- df_year[which.min(df_year$estimate), ]
    if (min_row$estimate > 0) {
      lowest_text <- sprintf(
        paste(
          "De laagste dichtheid in %s werd gevonden in %s in %s, %s SBP",
          "(%.2f [%.2f–%.2f] broedparen/100 ha)."
        ),
        y, min_row$regio, min_row$openheid, min_row$sbp,
        min_row$estimate, min_row$lcl, min_row$ucl
      )
      text_parts <- c(text_parts, lowest_text)
    }
  }

  # Compare against policy thresholds if provided
  if ("effect" %in% names(data) && "threshold" %in% names(data)) {
    t <- unique(data$threshold)

    # Case: all under threshold
    if (all(data$effect == "onder")) {
      text_parts <- c(
        text_parts,
        sprintf(
          paste(
            "Als we vergelijken met de streefwaarde van",
            "@agentschapvoornatuurenbos2021, nl. %s broedparen per 100 ha,",
            "zien we dat alle densiteiten significant lager zijn dan de",
            "streefwaarde."
          ),
          gsub("\\.", ",", t)
        )
      )
    } else {
      # Start threshold section
      threshold_text <- sprintf(
        paste("We vergelijken met de streefwaarde van",
              "@agentschapvoornatuurenbos2021, nl. %s broedparen per 100 ha."),
        gsub("\\.", ",", t)
      )
      text_parts <- c(text_parts, threshold_text)

      # Higher than threshold
      boven_rows <- data[data$effect == "boven", ]
      if (nrow(boven_rows) > 0) {
        boven_text <- apply(boven_rows, 1, function(row) {
          sprintf(
            paste("De densiteit ligt significant hoger dan de streefwaarde",
                  "in %s in %s in %s, %s SBP"),
            row[["year"]], row[["regio"]], row[["openheid"]], row[["sbp"]]
          )
        })
        text_parts <- c(text_parts, paste(boven_text, collapse = "; "))
      } else {
        text_parts <- c(
          text_parts,
          paste("De densiteiten liggen in geen enkel geval significant hoger",
                "dan de streefwaarde.")
        )
      }

      # No significant difference
      geen_rows <- data[data$effect == "geen sign.\nverschil", ]
      if (nrow(geen_rows) > 0) {
        geen_text <- apply(geen_rows, 1, function(row) {
          sprintf("%s in %s in %s, %s SBP", row[["year"]], row[["regio"]],
                  row[["openheid"]], row[["sbp"]])
        })
        text_parts <- c(
          text_parts,
          "We zien geen significant verschil met de streefwaarde in",
          paste0(paste(geen_text, collapse = "; "), ".")
        )
      }

      # Remaining cases are under
      text_parts <- c(
        text_parts,
        paste("In alle andere gevallen is de densiteit significant lager dan",
              "de streefwaarde.")
      )
    }
  }

  # Combine into one continuous text
  paste(text_parts, collapse = " ")
}
