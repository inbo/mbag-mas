fit_ds_models <- function(data, formulas, keys, ...) {
  require("Distance")

  # Create all combinations
  combinations <- expand.grid(
    formula = formulas,
    key = keys,
    stringsAsFactors = FALSE
  )

  # Convert to list of lists
  rules <- apply(combinations, 1, function(row) {
    list(formula = row[["formula"]], key = row[["key"]])
  })

  lapply(rules, function(rule) {
    f <- rule$formula
    k <- rule$key
    if (f == "~1") {
      ds_model <- ds(
        data = data,
        key = k,
        formula = as.formula(f),
        max_adjustments = 2,
        monotonicity = "strict",
        ...
      )
    } else {
      ds_model <- ds(
        data = data,
        key = k,
        formula = as.formula(f),
        adjustment = NULL,
        ...
      )
    }
    return(ds_model)
  })
}
