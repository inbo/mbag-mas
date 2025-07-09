list(
  tar_target(
    allocatie_df,
    allocatie(
      steekproefkader = steekproefkader_finaal,
      min_samplesize = 0,
      target_samplesize = 91,
      popsize_minimum = 0,
      allocatie_binnen_sbp = steekproefkader_finaal %>%
        st_drop_geometry() %>%
        count(is_sbp) %>%
        mutate(prop = n / sum(n)) %>%
        filter(is_sbp) %>%
        pull(prop),
      allocatie_leemstreek = 0,
      ol_strata = c("OL", "HOL")
    ),
    pattern = map(steekproefkader_finaal)
  ),
  tarchetypes::tar_group_by(
    allocatie_per_stratum,
    allocatie_df,
    Naam
  ),
  tarchetypes::tar_group_by(
    steekproefkader_per_stratum,
    do.call(rbind.data.frame, steekproefkader_finaal) %>%
      semi_join(allocatie_df %>%
                  select(Naam, is_sbp, openheid_klasse),
                by = c("Naam", "is_sbp", "openheid_klasse")),
    Naam
  ),
  tar_target(
    steekproef,
    draw_sample(
      sampling_frame = steekproefkader_per_stratum,
      sample_size = 91,
      sample_size_multiplication = 1,
      balance = c("X", "Y")
    ),
    pattern = map(steekproefkader_per_stratum, allocatie_per_stratum),
    iteration = "list"
  )
)
