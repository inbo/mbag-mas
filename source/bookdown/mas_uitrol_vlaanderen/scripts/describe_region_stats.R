#' Describe mean and standard deviation per region in Dutch
#'
#' Generates a grammatically correct Dutch sentence that describes the mean
#' and standard deviation of a numeric variable per region (or category).
#' The regions are ordered by their mean values (ascending), and the sentence
#' uses `"gevolgd door"` before the second element and `"en"` before the last
#' element.
#'
#' @param df A data frame or tibble containing at least the columns specified
#'   in `var_col`, `mean_col`, and `sd_col`.
#' @param lab A character string describing the variable, which will appear
#'   in the Dutch sentence (e.g., `"aantal soorten per telpunt"`).
#' @param var_col Name of the column with region or category names.
#'  Default is `"regio"`.
#' @param mean_col Name of the column with mean values.
#'  Default is `"mean"`.
#' @param sd_col Name of the column with standard deviation values.
#'  Default is `"sd"`.
#'
#' @return A single character string in Dutch describing the order of regions
#'   by mean value, including standard deviations.
describe_region_stats <- function(
  df,
  lab,
  var_col = "regio",
  mean_col = "mean",
  sd_col = "sd"
) {
  require("dplyr")
  require("rlang")

  # required columns
  required <- c(var_col, mean_col, sd_col)
  if (!all(required %in% names(df))) {
    stop("Dataframe must contain columns: ", paste(required, collapse = ", "))
  }

  # sort and format
  df2 <- df %>%
    arrange(.data[[mean_col]]) %>%
    mutate(
      mean_num = as.numeric(.data[[mean_col]]),
      sd_num   = as.numeric(.data[[sd_col]]),
      formatted = sprintf(
        "de %s (%s met SD van %s)",
        .data[[var_col]],
        formatC(.data$mean_num, format = "f", digits = 2, decimal.mark = ","),
        formatC(.data$sd_num,   format = "f", digits = 2, decimal.mark = ",")
      )
    )

  parts <- df2$formatted
  n <- length(parts)

  if (n == 0) return(NA_character_)
  if (n == 1) return(sprintf("Het laagste gemiddelde %s kwam voor in %s.", lab,
                             parts[1]))

  if (n == 2) {
    body <- paste("gevolgd door", parts[2])
    body <- paste(parts[1], body)
  } else {
    # first element stands alone
    first <- parts[1]
    # second element gets "gevolgd door"
    second <- paste("gevolgd door", parts[2])
    # remaining elements stay unchanged
    rest <- parts[3:n]

    # build the final string
    if (length(rest) > 0) {
      body <- paste(
        paste(first, second, sep = ", "),
        paste(rest, collapse = ", "),
        sep = ", "
      )
    } else {
      body <- paste(first, second, sep = ", ")
    }
  }

  sprintf("Het laagste gemiddelde %s komt voor in %s.", lab, body)
}
