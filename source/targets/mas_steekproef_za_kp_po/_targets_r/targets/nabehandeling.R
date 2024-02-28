list(
  tar_target(
    steekproef_thinned,
    thin_sample(
      sample = steekproef,
      thin_dist = 600
      ),
    pattern = map(steekproef),
    iteration = "list"
  ),
  tar_target(
    steekproef_final,
    replace_by_existing(
      sample = steekproef_thinned,
      existing_points = existing_data,
      id_existing_points = as.name("naam"),
      gebied = perimeters_data,
      overlap_prop = 0.5,
      sbp_file = do.call(rbind.data.frame, sbp_akkervogels)
      ),
    pattern = map(steekproef_thinned),
    iteration = "list"
  )
)
