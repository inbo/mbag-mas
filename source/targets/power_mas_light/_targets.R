################################################################################
# targets pipeline for power analysis MAS light                                #
# see https://books.ropensci.org/targets/                                      #
################################################################################

# Load packages required to define the pipeline
library(targets)
library(tarchetypes)
library(dplyr)
library(readr)

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

# Globals
mbag_dir <- rprojroot::find_root_file(criterion = rprojroot::is_git_root)

target_species <- c(
  "Boerenzwaluw",
  "Geelgors",
  "Gele Kwikstaart",
  "Grasmus",
  "Graspieper",
  "Grutto",
  "Kievit",
  "Kneu",
  "Patrijs",
  "Ringmus",
  "Roodborsttapuit",
  "Scholekster",
  "Torenvalk",
  "Veldleeuwerik",
  "Wulp"
)

# Source custom functions
tar_source()

# Target list
list(
  ## Prepare breeding dates data
  # Read file
  tar_file(
    name = mas_data_clean_file,
    command = file.path(mbag_dir, "output", "datasets", "mas_data_clean.csv")
  ),
  tar_target(
    name = mas_data_clean,
    command = read_csv(mas_data_clean_file, show_col_types = FALSE)
  )
)
