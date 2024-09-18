list(
  tar_target(
    output_objecten,
    output_finaal(
      files = list(steekproefkader = do.call(rbind.data.frame,
                                             steekproefkader_finaal),
                   steekproef = do.call(rbind.data.frame,
                                        steekproef_final)),
      write_out = TRUE
    )
  )
)
