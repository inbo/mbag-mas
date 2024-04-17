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
  "source", "targets", "data_preparations",
  criterion = rprojroot::is_git_root)
mbag_dir <- rprojroot::find_root_file(
  criterion = rprojroot::is_git_root)

# Source custom functions
# source(file.path(mbag_dir, "source", "R", ".R"))

# Target list
list(
  tar_target(
    name = data,
    command = tibble(x = rnorm(100), y = rnorm(100))
  ),
  tar_target(
    name = model,
    command = coefficients(lm(y ~ x, data = data))
  )
)
