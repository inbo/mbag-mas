predict_det_probs <- function(ds_model, ...) {
  # Extract data
  data <- ds_model$ddf$ds$aux$ddfobj$xmat

  # Extract ddf object
  ddf_object <- ds_model$ddf$ds$aux$ddfobj

  # Calculate observation specific detection probabilities
  det_probs <- sapply(seq_len(nrow(data)), function(i) {
    mrds::detfct(
      distance = data$distance[i],
      ddfobj = ddf_object,
      index = i,
      ...
    )
  })

  # Add detection probabilities to dataframe
  data$det_prob <- det_probs

  return(data)
}
