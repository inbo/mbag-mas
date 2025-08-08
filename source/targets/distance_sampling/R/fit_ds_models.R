fit_ds_models <- function(data, formulas, keys, ...) {
  require("Distance")

  # Create all combinations
  combinations <- expand.grid(
    formula = formulas,
    key = keys,
    stringsAsFactors = FALSE
  )

  # Convert each combination to a list
  rules <- apply(combinations, 1, function(row) {
    list(formula = row[["formula"]], key = row[["key"]])
  })

  # Try to fit all models, return NA if fitting fails
  results <- lapply(rules, function(rule) {
    f <- rule$formula
    k <- rule$key

    # Try fitting model with error handling
    tryCatch({
      if (f == "~1") {
        ds(
          data = data,
          key = k,
          formula = as.formula(f),
          max_adjustments = 2,
          monotonicity = "strict",
          ...
        )
      } else {
        ds(
          data = data,
          key = k,
          formula = as.formula(f),
          adjustment = NULL,
          ...
        )
      }
    }, error = function(e) {
      NA  # Return NA on error
    })
  })
  # Set list names
  names(results) <- sapply(
    rules,
    function(i) paste(i$formula, i$key, sep = ", ")
  )

  return(results)
}
