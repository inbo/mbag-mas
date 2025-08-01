options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("dplyr",
                            "readr",
                            "tidyr",
                            "tibble",
                            "nngeo",
                            "osmextract",
                            "terra",
                            "exactextractr",
                            "arrow",
                            "httr",
                            "sf",
                            "GVI"),
               format = "qs",
               memory = "transient",
               garbage_collection = TRUE,
               workspace_on_error = TRUE)

target_dir <- rprojroot::find_root_file(
  "source", "targets", "mas_steekproef_weidestreek",
  criterion = rprojroot::is_git_root
)
mbag_dir <- rprojroot::find_root_file(criterion = rprojroot::is_git_root)

source(file.path(mbag_dir, "source", "R", "landuse_maps.R"))
source(file.path(mbag_dir, "source", "R", "geocomputations.R"))
source(file.path(mbag_dir, "source", "R", "draw_sample.R"))
source(file.path(mbag_dir, "source", "R", "steekproefkader.R"))
source(file.path(mbag_dir, "source", "R", "berekening_hulpvariabelen.R"))
source(file.path(mbag_dir, "source", "R", "steekproeftrekking_nabehandeling.R"))
source(file.path(mbag_dir, "source", "R", "wfs_wcs.R"))
