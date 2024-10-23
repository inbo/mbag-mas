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
source(file.path(mbag_dir, "source", "R", "taxon_mapping.R"))


# Download MAS pipeline data

## ... to be continued

# Target list
list(
  # 1. Read in observation data
  ## We use "dynamic branching" in the targets pipeline.
  ## The pipeline creates new targets at runtime for each file.
  ## When we add a new dataset file for a certain year, the pipeline will only
  ## do calculations for the data of that year and not again for the other years
  ## if nothing changed there.

  # Get file paths for each year
  tarchetypes::tar_files_input(
    name = mas_counts_sovon_files,
    files = paths_to_counts_sovon(
      proj_path = target_dir
    )
  ),
  # Read data from file paths
  tar_target(
    name = mas_counts_sovon,
    command = sf::st_read(
      dsn = mas_counts_sovon_files,
      quiet = TRUE
    ),
    pattern = map(mas_counts_sovon_files),
    iteration = "list"
  ),
  # Convert Amersfoord to Lambert coordinates
  tar_target(
    name = crs_pipeline,
    command = amersfoort_to_lambert72(
      mas_counts_sovon
    ),
    pattern = map(mas_counts_sovon),
    iteration = "list"
  ),

  # 2. Read in sample points of MBAG MAS

  # Get file path
  tarchetypes::tar_file(
    name = sample_file,
    command = path_to_samples(
      proj_path = mbag_dir,
      file = "steekproef_avimap_mbag_mas.csv"
    )
  ),
  # Read table from file path
  tar_target(
    name = sample,
    command = readr::read_csv(
      file = sample_file,
      show_col_types = FALSE
    )
  ),
  # Select locations in MAS data that belong to sample points of MBAG MAS
  # We still branch per year
  tar_target(
    name = select_sampled_points,
    command = join_with_sample(
      crs_pipeline,
      sample
    ),
    pattern = map(crs_pipeline),
    iteration = "list"
  ),

  # 3. Data selection and preparation steps

  # Select data that fall within valid time periods
  tar_target(
    name = select_time_periods,
    command = select_within_time_periods(
      counts_df = select_sampled_points
    ),
    pattern = map(select_sampled_points),
    iteration = "list"
  ),
  # Calculate distances to observer
  tar_target(
    name = calculate_obs_distance,
    command = calculate_obs_dist(
      counts_df = select_time_periods
    ),
    pattern = map(select_time_periods),
    iteration = "list"
  ),
  # Select data that fall within the sampling unit circles
  tar_target(
    name = select_within_radius,
    command = calculate_obs_distance %>%
      filter(.data$distance2plot <= 300),
    pattern = map(calculate_obs_distance),
    iteration = "list"
  ),
  # Select data for birds and mammals
  tar_target(
    name = select_species_groups,
    command = dplyr::filter(
      select_within_radius,
      soortgrp %in% 1:2
    ),
    pattern = map(select_within_radius),
    iteration = "list"
  ),
  # Remove data from counts that were performed twice within the same time
  # period
  tar_target(
    name = remove_double_counts,
    command = process_double_counted_data(
      counts_df = select_species_groups
    ),
    pattern = map(select_species_groups),
    iteration = "list"
  ),
  # Set all taxon names to species level
  tar_target(
    name = remove_subspecies_names,
    command = adjust_subspecies_names_nl(
      counts_df = remove_double_counts
    ),
    pattern = map(remove_double_counts),
    iteration = "list"
  ),
  # Stop branching over years, bind all data together
  tar_target(
    name = mas_data_full,
    command = do.call(
      what = rbind.data.frame,
      args = c(remove_subspecies_names, make.row.names = FALSE)
    )
  ),
  # Remove unwanted columns
  tar_target(
    name = mas_data_clean,
    command = remove_columns(mas_data_full)
  ),

  # 4. Prepare data for publication on GBIF

  # Stop branching over years, bind all data together
  # Complete dataset from beginning pipeline
  tar_target(
    name = complete_data_crs,
    command = do.call(
      what = rbind.data.frame,
      args = c(crs_pipeline, make.row.names = FALSE)
    )
  ),
  # Add non-MAS data to MAS data for GBIF publication
  # Column mas_sample indicates whether the observation is part of the
  # MBAG - MAS data or not
  tar_target(
    name = complete_data_gbif_raw,
    command = rbind_all_mas_data(
      sample_data = mas_data_clean,
      extra_data = complete_data_crs
    )
  ),
  # Perform mapping of Darwin Core column names
  tar_target(
    name = darwincore_mapping,
    command = dwc_mapping(complete_data_gbif_raw)
  ),
  # Get taxon names and split dataframe in groups of `size`
  tarchetypes::tar_group_size(
    name = prepare_taxon_mapping,
    command = darwincore_mapping %>%
      dplyr::distinct(
        .data$dwc_taxonID,
        .data$dwc_vernacularName,
        .data$dwc_class,
        .data$dwc_kingdom) %>%
      dplyr::arrange(dwc_vernacularName),
    size = 50
  ),
  # Get taxonomic info from GBIF tax. backbone, branch over the groups to limit
  # the number of connections to GBIF backbone at once
  tar_target(
    name = taxon_mapping,
    command = map_taxa_from_vernacular(
      vernacular_name_df = prepare_taxon_mapping,
      vernacular_name_col = "dwc_vernacularName",
      out_cols = c("scientificName", "phylum", "order", "family", "genus",
                   "species", "authorship", "rank", "key"),
      filter_cols = list(class = "dwc_class", kingdom = "dwc_kingdom"),
      lang = "nld",
      limit = 1000,
      increment = 250
    ),
    pattern = map(prepare_taxon_mapping)
  ),
  # Add taxon names manual if required
  tar_target(
    name = manual_taxon_mapping,
    command = map_taxa_manual(
      taxonomy_df = taxon_mapping,
      manual_taxon_list = list(
        "Huismuis (zoogdier)" = 7429082,        # species
        "Barmsijs (Grote of Kleine)" = 6782561, # genus
        "Veldmuis/Aardmuis" = 2438591,          # genus
        "Wezel/Hermelijn" = 2433922,            # genus
        "groene kikker-complex" = 2426629,      # genus
        "rat spec." = 2439223,                  # genus
        "spitsmuis spec." = 5534                # family
      ),
      vernacular_name_col = "dwc_vernacularName",
      out_cols = c("scientificName", "phylum", "order", "family", "genus",
                   "species", "authorship", "rank", "key")
    )
  ),
  # Join taxon names and sort columns
  tar_target(
    name = dwc_mapping_final,
    command = finalise_dwc_df(
      data_df = darwincore_mapping,
      taxonomy_df = manual_taxon_mapping
    )
  ),
  # Write out GBIF dataset
  tar_target(
    name = create_dwc_csv,
    command = create_output_csv(
      x = dwc_mapping_final,
      file = "mas_data_vlaanderen",
      suffix_by = "year",
      path = file.path(mbag_dir, "output", "datasets")
    )
  )
)
