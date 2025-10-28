# Load functions
source(file.path("scripts", "autoqmd_cleanup.R"))

autoqmd_finalise <- function(qmd_files, message, child_dirs_rm = NULL) {
  # Clean up qmd
  lapply(qmd_files, autoqmd_cleanup, message = message) # nolint: object_usage_linter

  # Remove child documents folder
  unlink(child_dirs_rm, recursive = TRUE)
}

# Globals
files <- c("09_densiteitsschattingen.qmd", "detection_curves.qmd",
           "densities_tables.qmd")

msg <- c(
  "",
  "> ⚠️ **Let op:** de inhoud hieronder wordt automatisch",
  "> gegenereerd tijdens het renderen via `render_children.R`.",
  "> Pas dit deel niet manueel aan."
)

# -------------------------------
# Revert qmd and remove children
# -------------------------------
autoqmd_finalise(
  qmd_files = files,
  message = msg,
  child_dirs_rm = "spec_files2"
)

# Flanders qmd post render
flandersqmd::post_render()
