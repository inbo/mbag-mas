trend_to_beta_param <- function(
  trend_buiten,
  verschil_binnen = 0.02,
  min_trend_binnen = 1.01
  ) {
  if (trend_buiten + verschil_binnen <= min_trend_binnen) {
    return(log(min_trend_binnen / trend_buiten))
  }

  log((trend_buiten + verschil_binnen) / trend_buiten)
}
