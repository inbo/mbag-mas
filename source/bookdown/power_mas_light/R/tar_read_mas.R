tar_read_mas <- function(...) {
  mbag_dir <- rprojroot::find_root_file(criterion = rprojroot::is_git_root)
  store_path <- file.path(mbag_dir, "source", "targets", "power_mas_light",
                          "_targets")
  targets::tar_read(..., store = store_path)
}
