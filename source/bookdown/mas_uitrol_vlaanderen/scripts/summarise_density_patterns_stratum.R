#' Summarise distance sampling density patterns per stratum
#'
#' Generates a narrative description of density estimates across regions,
#' highlighting general patterns (e.g. higher densities in OL vs HOL, or binnen
#' vs buiten SBP across most regions) and then adding region-specific detail
#' when no clear general pattern emerges.
#'
#' @param data A data frame with columns: \code{regio}, \code{openheid},
#'   \code{sbp}, and \code{estimate}.
#' @param species Optional character string giving the species name for the
#'   description.
#' @param year Optional numeric or character year label.
#' @param threshold Numeric ratio used to define a "remarkable" difference
#'   (default = 1.5, i.e. ≥50% higher).
#'
#' @return A single character string with a continuous text description.
summarise_density_patterns_stratum <- function(
    data,
    species = NULL,
    year = NULL,
    threshold = 1.5
  ) {
  stopifnot(all(c("regio", "openheid", "sbp", "estimate") %in% names(data)))

  # Compare OL vs HOL per region
  ol_vs_hol <- tapply(seq_len(nrow(data)), data$regio, function(idx) {
    df <- data[idx, ]
    mean_ol <- mean(df$estimate[df$openheid == "OL"], na.rm = TRUE)
    mean_hol <- mean(df$estimate[df$openheid == "HOL"], na.rm = TRUE)
    if (!is.na(mean_ol) && !is.na(mean_hol)) {
      if (mean_ol > threshold * mean_hol) return("OL")
      if (mean_hol > threshold * mean_ol) return("HOL")
    }
    return("similar")
  })

  # Compare binnen vs buiten per region
  bin_vs_bui <- tapply(seq_len(nrow(data)), data$regio, function(idx) {
    df <- data[idx, ]
    mean_bin <- mean(df$estimate[df$sbp == "binnen"], na.rm = TRUE)
    mean_bui <- mean(df$estimate[df$sbp == "buiten"], na.rm = TRUE)
    if (!is.na(mean_bin) && !is.na(mean_bui)) {
      if (mean_bin > threshold * mean_bui) return("binnen")
      if (mean_bui > threshold * mean_bin) return("buiten")
    }
    return("similar")
  })

  # Detect general patterns
  ol_majority <- names(which.max(table(ol_vs_hol)))
  bin_majority <- names(which.max(table(bin_vs_bui)))

  n_regions <- length(unique(data$regio))
  text_parts <- list()

  # Header
  if (!is.null(species) && !is.null(year)) {
    text_parts <- c(text_parts, sprintf("Voor %s in %s werden de volgende patronen gevonden:", species, year))
  } else if (!is.null(species)) {
    text_parts <- c(text_parts, sprintf("Voor %s werden de volgende patronen gevonden:", species))
  } else if (!is.null(year)) {
    text_parts <- c(text_parts, sprintf("In %s werden de volgende patronen gevonden:", year))
  } else {
    text_parts <- c(text_parts, "De dichtheden vertoonden de volgende patronen:")
  }

  # General pattern: OL vs HOL
  if (ol_majority != "similar" && sum(ol_vs_hol == ol_majority) > n_regions / 2) {
    if (ol_majority == "OL") {
      text_parts <- c(text_parts, "In de meeste regio’s lagen de dichtheden duidelijk hoger in OL dan in HOL.")
    } else {
      text_parts <- c(text_parts, "In de meeste regio’s lagen de dichtheden duidelijk hoger in HOL dan in OL.")
    }
  }

  # General pattern: binnen vs buiten SBP
  if (bin_majority != "similar" && sum(bin_vs_bui == bin_majority) > n_regions / 2) {
    if (bin_majority == "binnen") {
      text_parts <- c(text_parts, "Ook lagen de dichtheden in de meerderheid van de regio’s hoger binnen SBP dan buiten SBP.")
    } else {
      text_parts <- c(text_parts, "Ook lagen de dichtheden in de meerderheid van de regio’s hoger buiten SBP dan binnen SBP.")
    }
  }

  # If no clear majority patterns: give regional detail
  if ((ol_majority == "similar" || sum(ol_vs_hol == ol_majority) <= n_regions / 2) &&
      (bin_majority == "similar" || sum(bin_vs_bui == bin_majority) <= n_regions / 2)) {
    regional_texts <- mapply(function(region, ol, bin) {
      desc_ol <- if (ol == "OL") "OL was hoger dan HOL" else if (ol == "HOL") "HOL was hoger dan OL" else "OL en HOL waren vergelijkbaar"
      desc_bin <- if (bin == "binnen") "binnen SBP lag hoger dan buiten SBP" else if (bin == "buiten") "buiten SBP lag hoger dan binnen SBP" else "binnen en buiten SBP waren vergelijkbaar"
      sprintf("In %s: %s, en %s.", region, desc_ol, desc_bin)
    }, names(ol_vs_hol), ol_vs_hol, bin_vs_bui, SIMPLIFY = TRUE)

    text_parts <- c(text_parts, regional_texts)
  }

  # Collapse into one continuous text
  paste(text_parts, collapse = " ")
}
