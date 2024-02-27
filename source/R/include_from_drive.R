# Functie om bestanden van google drive te downloaden (voor rapporten)
include_from_drive <- function(name, figures = figs, overwrite = FALSE, ...) {
  common_part <- fs::path_common(figures$path)
  the_fig <- figures[figures$name == name, ]
  full_path <- the_fig$path
  rel_path <- fs::path_rel(full_path, common_part)
  local_path <- fs::path("media", rel_path)
  # maak de folderstructuur als deze nog niet bestaat
  fs::dir_create(fs::path_dir(local_path))
  # met overwrite = FALSE, zal de functie een error geven als de file al bestaat
  # daarom twee gevallen toelaten:
  # 1: overwrite = TRUE
  if (overwrite) {
    df <- drive_download(
      file = the_fig,
      path = local_path,
      overwrite = overwrite)
  }
  # 2: bestand bestaat nog niet, download het
  if (!file.exists(local_path)) {
    df <- drive_download(
      file = the_fig,
      path = local_path,
      overwrite = overwrite)
  }
  knitr::include_graphics(local_path, ...)
}
