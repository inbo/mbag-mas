################################################################################
# targets pipeline for reading, cleaning and preparation of the MBAG MAS data  #
# see https://books.ropensci.org/targets/                                      #
################################################################################

# Load packages required to define the pipeline
library(targets)
library(tarchetypes)

# Set target options
tar_option_set(
  packages = c("tidyverse",
               "sf"),
  format = "qs",
  memory = "transient",
  garbage_collection = TRUE,
  workspace_on_error = TRUE
)

# Set directory locations
target_dir <- rprojroot::find_root_file(
  "source", "targets", "data_preparation",
  criterion = rprojroot::is_git_root)
mbag_dir <- rprojroot::find_root_file(
  criterion = rprojroot::is_git_root)

# Source custom functions
lapply(list.files(file.path(target_dir, "R"), full.names = TRUE), source)

# Target list
list(
  tarchetypes::tar_file(
    mas_counts_sovon_file,
    read_counts_sovon(mbag_dir, "20230810_qgis_export_sovon_wfs_2023.geojson")
  ),
  tar_target(
    name = crs_pipeline,
    command = amersfoort_to_lambert72(mas_counts_sovon_file)
  )
)
