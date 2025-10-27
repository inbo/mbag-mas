#' Describe trends in coverage over time
#'
#' This function generates short textual descriptions of observed trends in
#' vegetation (or other coverage categories) per year based on a summary table.
#' For each year, it reports the dominant trend type(s), the least frequent
#' trend(s), whether trends tend to be simple or complex, and which trend
#' categories (if any) are missing.
#'
#' @param df A data frame containing trend information with at least the
#' columns:
#'   \itemize{
#'     \item `year`: numeric or factor year value
#'     \item `trend`: character string describing the trend category
#'     \item `n`: numeric count of observations for that trend in the given
#'           year
#'     \item `complexity`: numeric value describing the complexity level of
#'           the trend
#'   }
#' @param all_trends A character vector listing all possible trend categories.
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
    mutate(y = .data$year) %>%
    group_by(.data$year) %>%
    group_map(~{
      data_year <- .x  # Subset of data for one year

      # Compute total observations and average complexity
      total <- sum(data_year$n)
      avg_complex <- weighted.mean(data_year$complexity, data_year$n)

      # Decide if trends are generally more simple or complex
      complexity_level <- ifelse(avg_complex > 1.5, "meer complexe",
                                 "meer eenvoudige")

      # Identify most common and least common trends
      most_common <- data_year %>%
        filter(n == max(n))
      least_common <- data_year %>%
        filter(n == min(n))

      # Convert trend names to comma-separated text
      most_common_vec <- paste(most_common$trend, collapse = ", ")
      least_common_vec <- paste(least_common$trend, collapse = ", ")

      # Identify missing trend categories
      missing <- setdiff(all_trends, data_year$trend)
      missing_vec <- paste(missing, collapse = ", ")

      # Adjust grammar depending on singular/plural cases
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

      # Compose the descriptive text for this year
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


#' Describe and compare NDVI and BSI summary tables
#'
#' This function produces natural-language descriptions (in Dutch) of
#' summary statistics for the percentage of bare soil (`perc_bare_soil`)
#' across categories (e.g., habitat type, region, etc.), comparing two
#' indicators: NDVI and BSI. For each indicator, the function reports which
#' categories have the highest or lowest average bare soil percentage and
#' whether the order of categories is consistent between indicators.
#'
#' @param summary_data A list or data object containing both NDVI and BSI data
#'   frames, each with at least the columns:
#'   \itemize{
#'     \item `perc_bare_soil`: numeric, percentage of bare soil
#'     \item the grouping variable given in `var`, e.g. habitat or stratum
#'   }
#'   Typically this would be a list such as
#'   `list(ndvi = ndvi_df, bsi = bsi_df)`.
#' @param var A character string giving the column name to group by (e.g.
#'  `"habitat"`).
#' @param tol Numeric tolerance (default = 0.05) used to decide if two means
#'   are considered similar between NDVI and BSI or between categories.
#'
#' @return A character string containing a Dutch-language description of the
#'   trends and their similarities or differences between NDVI and BSI.
describe_summary_tables <- function(summary_data, var, tol = 0.05) {
  require("dplyr")
  require("rlang")
  require("glue")

  #--------------------------------------------------------------------
  # Helper function to summarise percentage of bare soil for one indicator
  #--------------------------------------------------------------------
  summarise_bare_soil <- function(data, var) {
    data %>%
      group_by(.data[[var]]) %>%
      summarise(
        min_bs    = min(.data$perc_bare_soil, na.rm = TRUE),
        q25_bs    = quantile(.data$perc_bare_soil, 0.25, na.rm = TRUE),
        median_bs = median(.data$perc_bare_soil, na.rm = TRUE),
        q75_bs    = quantile(.data$perc_bare_soil, 0.75, na.rm = TRUE),
        max_bs    = max(.data$perc_bare_soil, na.rm = TRUE),
        mean_bs   = mean(.data$perc_bare_soil, na.rm = TRUE),
        sd_bs     = sd(.data$perc_bare_soil, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(.data$mean_bs))
  }

  #--------------------------------------------------------------------
  # Compute summaries for both indicators (NDVI and BSI)
  #--------------------------------------------------------------------
  ndvi_summary <- summarise_bare_soil(summary_data$ndvi, var)
  bsi_summary  <- summarise_bare_soil(summary_data$bsi, var)

  #--------------------------------------------------------------------
  # Helper function to check if NDVI and BSI orders are similar
  # within tolerance (for simple cases with two categories)
  #--------------------------------------------------------------------
  same_order_tol <- function(ndvi_df, bsi_df, tol) {
    # If order is identical, return TRUE
    if (identical(ndvi_df[[var]], bsi_df[[var]])) {
      return(TRUE)
    } else {
      # Special handling for only two categories
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

  #--------------------------------------------------------------------
  # Helper to describe one indicator’s results in natural Dutch
  #--------------------------------------------------------------------
  describe_one <- function(summary_df, indicator_label) {
    # Format each row as a text snippet with mean, median, and SD
    var_text <- apply(summary_df, 1, function(row) {
      sprintf("%s (gemiddelde: %.3f, mediaan: %.3f, SD: %.3f)",
              row[[var]],
              as.numeric(row[["mean_bs"]]),
              as.numeric(row[["median_bs"]]),
              as.numeric(row[["sd_bs"]]))
    })

    # Construct sentences depending on number of categories
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
      # For two categories, check if means are similar within tolerance
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
      stop("At least two categories are required.")
    }
  }

  #--------------------------------------------------------------------
  # Generate the final textual description
  #--------------------------------------------------------------------
  if (same_order) {
    # Case 1: NDVI and BSI show similar ordering across categories
    var_text <- mapply(
      function(n_row, b_row) {
        sprintf(
          paste("%s (NDVI – gemiddelde: %.3f, mediaan: %.3f, SD: %.3f;",
                "BSI – %.3f, %.3f, %.3f)"),
          n_row[[var]],
          n_row[["mean_bs"]], n_row[["median_bs"]], n_row[["sd_bs"]],
          b_row[["mean_bs"]], b_row[["median_bs"]], b_row[["sd_bs"]]
        )
      },
      split(ndvi_summary, seq_len(nrow(ndvi_summary))),
      split(bsi_summary, seq_len(nrow(bsi_summary)))
    )

    # Multi-category narrative
    if (nrow(ndvi_summary) > 2) {
      out_text <- sprintf(
        paste("De resultaten zijn vergelijkbaar tussen NDVI en BSI.",
              "Het hoogste percentage naakte bodem, gemiddeld over alle jaren,",
              "is in %s, gevolgd door %s."),
        var_text[1],
        paste(var_text[-1], collapse = ", ")
      )
    } else {
      # Two-category comparison with tolerance checks
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
                "is hoger in %s dan in %s."),
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
    # Case 2: NDVI and BSI show different ordering
    ndvi_text <- describe_one(ndvi_summary, "NDVI")
    bsi_text  <- describe_one(bsi_summary, "BSI")

    out_text <- glue(
      "De volgorde verschilt tussen de indicatoren. ",
      "{ndvi_text} {bsi_text}"
    )
  }

  return(out_text)
}
