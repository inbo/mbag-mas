#' Summarise distance sampling density patterns per region
#'
#' Generates a narrative summary of regional density estimates (e.g. for
#' distance sampling results) across years. The function lists all regions
#' (except those where the species is practically absent, i.e.
#' `estimate = 0` or `lcl < 0.1`) in order of decreasing density per
#' year.
#' If "Total" (or Vlaanderen) is present, it is mentioned separately at the end.
#' Optionally, comparisons with policy thresholds are included if
#' `effect` and `threshold` columns are available.
#'
#' @param data A data frame with the following required columns:
#'   \describe{
#'     \item{`region`}{Region name (character).}
#'     \item{`estimate`}{Estimated density (numeric).}
#'     \item{`lcl`}{Lower confidence limit (numeric).}
#'     \item{`ucl`}{Upper confidence limit (numeric).}
#'     \item{`species`}{Species name (character).}
#'     \item{`year`}{Year of the estimate (numeric or factor).}
#'   }
#'   Optionally:
#'   \describe{
#'     \item{effect}{Categorical: `"boven"`, `"onder"`, or
#'           `"geen sign.\nverschil"`.}
#'     \item{threshold}{Numeric policy target.}
#'   }
#'
#' @param threshold Numeric ratio used to define a "remarkable" difference
#'   (default = 1.5, i.e. ≥50% higher), used for threshold comparisons.
#'
#' @return A single character string containing the continuous narrative
#' summary.
summarise_region_densities <- function( # nolint: cyclocomp_linter
  data,
  threshold = 1.5
) {
  # Check required columns
  stopifnot(
    all(
      c("region", "estimate", "lcl", "ucl", "species", "year") %in% names(data)
    )
  )

  # Parse species
  spec <- tolower(unique(data$species))
  quantity_type <- ifelse(spec %in% tolower(roofvogels_f()),
                          "individuen", "broedparen")

  years <- sort(unique(data$year))
  regions <- sort(unique(data$region))

  # Identify regions that are practically absent across all years
  absent_regions <- regions[sapply(regions, function(r) {
    reg_rows <- data[data$region == r, ]
    all(reg_rows$estimate == 0 | reg_rows$lcl < 0.1)
  })]

  text_parts <- list()

  # Add statement for absent regions
  if (length(absent_regions) > 0 && length(absent_regions) != nrow(data)) {
    text_parts <- c(text_parts,
                    sprintf("De soort is praktisch afwezig in %s.",
                            paste(absent_regions, collapse = ", ")))
  }

  # Function to describe a single year
  describe_year <- function(subdata, year_label) {
    # Remove "Total" for the ranking part
    total_rows <- subdata[subdata$region %in% c("Total", "Vlaanderen"), ]
    df <- subdata[!subdata$region %in% c("Total", "Vlaanderen"), ]

    # Mark presence
    df <- df[df$estimate > 0 & df$lcl >= 0.1, ]
    if (nrow(df) == 0) {
      return(
        sprintf(
          "In %s was de soort in alle onderzochte regio's praktisch afwezig.",
          year_label
        )
      )
    }

    # Order by densiteit (highest first)
    df <- df[order(df$estimate, decreasing = TRUE), ]

    # Create regional listing
    regional_text <- apply(df, 1, function(row) {
      sprintf(paste("%s (%.2f [%.2f–%.2f]", quantity_type, "per 100 ha)"),
              row[["region"]], as.numeric(row[["estimate"]]),
              as.numeric(row[["lcl"]]), as.numeric(row[["ucl"]]))
    })

    # Compose text for this year
    if (length(regional_text) == 1) {
      year_text <- sprintf(
        "In %s was er enkel een relatief hoge densiteit in de %s.",
        year_label,
        regional_text[1]
      )
    } else {
      year_text <- sprintf(
        "In %s waren de hoogste densiteiten in %s, gevolgd door %s.",
        year_label,
        regional_text[1],
        paste(regional_text[-1], collapse = ", ")
      )
    }

    # If Total/Vlaanderen is present, mention separately
    if (nrow(total_rows) > 0) {
      tr <- total_rows[1, ]
      total_text <- sprintf(
        paste("Over heel Vlaanderen bedroeg de densiteit %.2f [%.2f–%.2f]",
              quantity_type, "per 100 ha."),
        tr$estimate, tr$lcl, tr$ucl
      )
      year_text <- paste(year_text, total_text)
    }

    year_text
  }

  # Add description per year
  for (y in years) {
    sub <- data[data$year == y, ]
    text_parts <- c(text_parts, describe_year(sub, y))
  }

  # Add threshold comparisons if available
  if ("effect" %in% names(data) && "threshold" %in% names(data)) {
    t <- unique(data$threshold)

    # Case: all under threshold
    if (all(data$effect == "onder")) {
      text_parts <- c(
        text_parts,
        sprintf(
          paste(
            "Als we vergelijken met de streefwaarde van",
            "@agentschapvoornatuurenbos2021, nl. %s",
            quantity_type, "per 100 ha,",
            "zien we dat alle densiteiten onder de streefwaarde liggen."
          ),
          gsub("\\.", ",", t)
        )
      )
    } else {
      # Start threshold section
      threshold_text <- sprintf(
        paste("We vergelijken met de streefwaarde van",
              "@agentschapvoornatuurenbos2021, nl. %s",
              quantity_type, "per 100 ha."),
        gsub("\\.", ",", t)
      )
      text_parts <- c(text_parts, threshold_text)

      # Higher than threshold
      boven_rows <- data[data$effect == "boven", ]
      if (nrow(boven_rows) > 0) {
        boven_text <- apply(boven_rows, 1, function(row) {
          sprintf(
            paste("De densiteit ligt significant hoger dan de streefwaarde",
                  "in %s in %s."),
            row[["year"]], row[["region"]]
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
          sprintf("%s in %s", row[["year"]], row[["region"]])
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
        paste(
          "In alle andere gevallen ligt de densiteit onder de streefwaarde."
        )
      )
    }
  }

  # Combine into one text block
  paste(text_parts, collapse = " ")
}
