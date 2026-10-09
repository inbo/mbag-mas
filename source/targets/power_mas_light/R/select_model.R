select_model <- function(model_list) {
  aic <- AIC(model_list$poisson, model_list$negbin)
  phi <- sigma(model_list$negbin)

  if (phi > 1e6) {
    return(list(family = "poisson", fit = model_list$poisson))
  } else if ((aic$AIC[2] + 2) < aic$AIC[1]) {
    return(list(family = "negbin", fit = model_list$negbin))
  } else {
    return(list(family = "poisson", fit = model_list$poisson))
  }
}
