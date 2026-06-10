library(flandersqmd)

message <- c(
  "",
  "> Niet manueel aanpassen"
)

# Clean report file and remove child files
autoqmd_finalise(
  target_files = "04_schatting_simulatieparameters.qmd",
  message = message,
  child_dirs_rm = "child_qmd"
)

# Flanders qmd post render
flandersqmd::post_render()
