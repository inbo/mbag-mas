prepare_design <- function(df, digits) {
  # Remove NA
  df <- df[colSums(!is.na(df)) > 0]

  digits[names(digits) %in% names(df)]

  list(
    design = as.list(df),
    digits = digits[names(digits) %in% names(df)]
  )
}
