list(
  tar_target(
    name = plus_sb,
    command = add_bo_to_frame(
      punten_df = punten_selectie_zichtbaarheid,
      path_bo = bo_file
    ),
    pattern = map(punten_selectie_zichtbaarheid),
    iteration = "list"
  ),
  tar_target(
    name = plus_openheid_landschap,
    command = add_openheid_landschap_to_frame(
      path = openheid_landschap_file,
      punten_sf = plus_sb,
      gebied = perimeters_data,
      cutlevels = c(1.25, 1.35, 1.51),
      class_labels = c("GL", "HGL", "HOL", "OL")
    ),
    pattern = map(perimeters_data, plus_sb),
    iteration = "list"
  ),
  tar_target(
    name = sbp_akkervogels,
    command = read_sbp_others(
      path = sbp_overige_file,
      soorten = c("hamster", "bruine kiekendief", "zomertortel",
                  "grauwe kiekendief"),
      gebied = perimeters_data
    ),
    pattern = map(perimeters_data),
    iteration = "list"
  ),
  tar_target(
    name = steekproefkader_finaal,
    command = add_stratum_sbp(
      punten_sf = plus_openheid_landschap,
      sbp = do.call(rbind.data.frame, sbp_akkervogels)
    ),
    pattern = map(plus_openheid_landschap),
    iteration = "list"
  )
)
