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
source(file.path(mbag_dir, "source", "R", "predatoren_f.R"))

# Target list
list(
  tarchetypes::tar_files(
    name = mas_counts_sovon_files,
    command = paths_to_counts_sovon(
      proj_path = target_dir
    )
  ),
  tar_target(
    name = mas_counts_sovon,
    command = sf::st_read(
      dsn = mas_counts_sovon_files,
      quiet = TRUE
    ),
    pattern = map(mas_counts_sovon_files),
    iteration = "list"
  ),
  tar_target(
    name = crs_pipeline,
    command = amersfoort_to_lambert72(
      mas_counts_sovon
    ),
    pattern = map(mas_counts_sovon),
    iteration = "list"
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
    command = readr::read_csv(
      file = sample_file,
      show_col_types = FALSE
    )
  ),
  tar_target(
    name = select_sampled_points,
    command = dplyr::inner_join(
      x = crs_pipeline,
      y = sample,
      by =  dplyr::join_by(plotnaam == pointid)
    ),
    pattern = map(crs_pipeline),
    iteration = "list"
  ),
  tar_target(
    name = select_time_periods,
    command = select_within_time_periods(
      counts_df = select_sampled_points
    ),
    pattern = map(select_sampled_points),
    iteration = "list"
  ),
  tar_target(
    name = select_within_radius,
    command = select_within_circle_radius(
      counts_df = select_time_periods,
      radius = 300
    ),
    pattern = map(select_time_periods),
    iteration = "list"
  ),
  tar_target(
    name = select_species_groups,
    command = dplyr::filter(
      select_within_radius,
      soortgrp %in% 1:2
    ),
    pattern = map(select_within_radius),
    iteration = "list"
  ),
  tar_target(
    name = remove_double_counts,
    command = process_double_counted_data(
      counts_df = select_species_groups
    ),
    pattern = map(select_species_groups),
    iteration = "list"
  ),
  tar_target(
    name = remove_subspecies_names,
    command = adjust_subspecies_names_nl(
      counts_df = remove_double_counts
    ),
    pattern = map(remove_double_counts),
    iteration = "list"
  ),
  tar_target(
    name = add_predator_variable,
    command = add_predator_variables(
      counts_df = remove_subspecies_names
    ),
    pattern = map(remove_subspecies_names),
    iteration = "list"
  ),
  tar_target(
    name = mas_data_clean,
    command = do.call(
      what = rbind.data.frame,
      args = c(add_predator_variable, make.row.names = FALSE)
    )
  )
)
