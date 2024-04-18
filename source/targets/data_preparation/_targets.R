################################################################################
# targets pipeline for reading, cleaning and preparation of the MBAG MAS data  #
# see https://books.ropensci.org/targets/                                      #
################################################################################

# Load packages required to define the pipeline
library(targets)
library(tarchetypes)
library(dplyr)
library(readr)
library(sf)

# Set target options
tar_option_set(
  packages = c("tidyverse",
               "sf"),
  format = "qs",
  memory = "transient",
  garbage_collection = TRUE,
  workspace_on_error = TRUE
)

# Conflicts
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::lag)

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
    name = mas_counts_sovon_file,
    command = path_to_counts_sovon(
      proj_path = mbag_dir,
      file = "20230810_qgis_export_sovon_wfs_2023.geojson"
      )
  ),
  tar_target(
    name = mas_counts_sovon,
    command = sf::st_read(
      mas_counts_sovon_file
      )
  ),
  tar_target(
    name = crs_pipeline,
    command = amersfoort_to_lambert72(
      mas_counts_sovon
      )
  ),
  tarchetypes::tar_file(
    name = sample_file,
    command = path_to_samples(
      proj_path = mbag_dir,
      file = "steekproef_avimap_mbag_piloot.csv"
      )
  ),
  tar_target(
    name = sample,
    command = readr::read_csv(sample_file)
  )
)
