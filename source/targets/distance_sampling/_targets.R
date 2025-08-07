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

# Set directory locations
target_dir <- rprojroot::find_root_file(
  "source", "targets", "distance_sampling",
  criterion = rprojroot::is_git_root
)
mbag_dir <- rprojroot::find_root_file(criterion = rprojroot::is_git_root)

# Source custom functions
lapply(list.files(file.path(target_dir, "R"), full.names = TRUE), source)
source(file.path(mbag_dir, "source", "R", "predatoren_f.R"))
source(file.path(mbag_dir, "source", "R", "summarize_ds_models2.R"))
source(file.path(mbag_dir, "source", "R", "beta_fit_params.R"))

# Replace the target list below with your own:
list(
  ## Prepare breeding dates data
  # Read file
  tar_file(
    name = breeding_dates_file,
    command = file.path(mbag_dir, "data", "SOVON",
                        "Interpretatie_Criteria_Broedvogels_v2.csv")
  ),
  tar_target(
    name = breeding_dates_raw,
    command = read_csv2(breeding_dates_file,
                        show_col_types = FALSE)
  ),
  # Clean up data
  tar_target(
    name = breeding_dates,
    command = parse_breeding_dates(breeding_dates_raw)
  ),

  ## Prepare sampling design data
  # Read file
  tar_file(
    name = full_sample_file,
    command = file.path(mbag_dir, "data", "steekproefkaders",
                        "steekproefkader_mbag_mas.gpkg")
  ),
  tar_target(
    name = full_sample,
    command = st_read(full_sample_file)
  ),
  # Calculate area per region
  tar_target(
    name = region_sf,
    command = full_sample %>%
      mutate(
        regio = ifelse(grepl("\\sleemstreek$", regio), "Leemstreek", regio)
      ) %>%
      st_buffer(dist = 300) %>%
      group_by(regio) %>%
      summarise(geom = st_union(geom)) %>%
      ungroup() %>%
      mutate(Area = as.numeric(st_area(geom)) / 1e6) %>%
      select(regio, Area, everything())
  ),
  # Calculate area for Flanders
  tar_target(
    name = flanders_sf,
    command = full_sample %>%
      st_buffer(dist = 300) %>%
      summarise(geom = st_union(geom)) %>%
      ungroup() %>%
      mutate(
        regio = "Flanders",
        Area = as.numeric(st_area(geom)) / 1e6
      ) %>%
      select(regio, Area, everything())
  ),
  # Calculate area per stratum
  tar_target(
    name = strata_sf,
    command = full_sample %>%
      mutate(
        regio = ifelse(grepl("\\sleemstreek$", regio), "Leemstreek", regio)
      ) %>%
      st_buffer(dist = 300) %>%
      group_by(regio, "openheid" = openheid_klasse, sbp) %>%
      summarise(geom = st_union(geom)) %>%
      ungroup() %>%
      mutate(Area = as.numeric(st_area(geom)) / 1e6) %>%
      select(regio, openheid, sbp, Area, everything())
  ),

  ## Prepare design for distance sampling
  # Read design
  tar_file(
    name = design_file,
    command = file.path(mbag_dir, "data", "steekproefkaders",
                        "steekproef_avimap_mbag_mas.csv")
  ),
  tar_target(
    name = design,
    command = read_csv(design_file, show_col_types = FALSE) %>%
      mutate(
        regio = ifelse(grepl("\\sleemstreek$", regio), "Leemstreek", regio)
      )
  ),
  # Prepare distance sampling tables
  tar_target(
    name = region_table,
    command = strata_sf %>%
      st_drop_geometry() %>%
      mutate(stratum = paste(openheid, sbp, sep = " - ")) %>%
      mutate(Region.Label = paste(regio, openheid, sbp, sep = " - ")) %>%
      select(Region.Label, Area)
  ),
  tar_target(
    name = sample_table,
    command = design %>%
      distinct(pointid, regio, openheid = openheid_klasse, sbp) %>%
      mutate(
        Region.Label = paste(regio, openheid, sbp, sep = " - "),
        Effort = 1 # assume one visit for maxima
      ) %>%
      select(Sample.Label = pointid, Region.Label, Effort)
  ),

  # Load occurrence data
  tar_target(
    name = mas_data_clean,
    command = tar_read(
      "mas_data_clean",
      store = file.path(mbag_dir, "source", "targets", "data_preparation",
                        "_targets")
    )
  ),

  ## Static branching over species
  tar_map(
    values = list(species = c(c("Veldleeuwerik", "Houtduif"))),

    ## Prepare species occurrence data
    # Select species and group occurrence data by year
    tar_group_by(
      name = mas_data_grouped,
      command = mas_data_clean %>%
        sf::st_drop_geometry() %>%
        filter(
          naam %in% species,
          jaar == 2024, # Change
        ) %>%
        mutate(
          regio = ifelse(grepl("\\sleemstreek$", regio), "Leemstreek", regio),
          stratum = paste(openheid_klasse, sbp, sep = " - ")
        ),
      jaar
    ),
    # Filter breeding codes:
    # > 0 for breeding birds
    # all for predators and mammals
    tar_target(
      name = filtered_breeding_code,
      command = mas_data_grouped %>%
        filter(
          (wrntype > 0 & !(naam %in% c("Haas", roofvogels_f()))) |
            naam %in% c("Haas", roofvogels_f())
        ),
      pattern = map(mas_data_grouped)
    ),
    # Filter within breeding dates
    tar_target(
      name = filtered_breeding_date,
      command = filter_breeding_date(
        filtered_breeding_code,
        dates = breeding_dates,
        exception = c("Haas", roofvogels_f())
      ),
      pattern = map(filtered_breeding_code)
    ),

    ## Prepare distance sampling data
    tar_group_by(
      name = ds_data,
      command = filtered_breeding_date %>%
        select(
          species = naam,
          year = jaar,
          object = oid,
          size = aantal,
          distance = distance2plot,
          openheid = openheid_klasse,
          sbp,
          regio,
          stratum
        ),
      year
    ),
    tar_group_by(
      name = obs_table,
      command = filtered_breeding_date %>%
        group_by(periode_in_jaar, plotnaam) %>%
        mutate(n = n()) %>%
        group_by(plotnaam) %>%
        slice_max(order_by = n, n = 1) %>%
        ungroup() %>%
        mutate(
          Region.Label = paste(
            regio,
            openheid = openheid_klasse,
            sbp,
            sep = " - "
          )
        ) %>%
        select("object" = "oid", "Region.Label", "Sample.Label" = "plotnaam",
               "species" = "naam", "year" = "jaar"),
      year
    ),
    tar_target(
      name = conversion_factor,
      command = Distance::convert_units("meter", NULL, "Square kilometer")
    ),

    ## Model specification
    # Get formulas
    tar_target(
      name = formulae,
      command = list(
        "~1",
        "~regio",
        #"~sbp",
        "~openheid"
        #"~regio+sbp",
        #"~regio+openheid",
        #"~sbp+openheid",
        #"~regio+sbp+openheid",
        #"~sbp*openheid"
      )
    ),
    # Fit models
    tar_target(
      name = ds_model_fits,
      command = fit_ds_models(
        data = ds_data,
        formulas = formulae,
        # Distance::ds arguments:
        keys = c("hn", "hr"),
        truncation = 300,
        transect = "point",
        dht_group = FALSE,
        convert_units = conversion_factor,
        region_table = region_table,
        sample_table = sample_table,
        obs_table = obs_table
      ),
      pattern = map(ds_data),
      iteration = "list"
    ),

    ## Model comparison
    # Get model fit measures
    tar_target(
      name = aic_comparison,
      command = summarize_ds_models2(ds_model_fits, output = "plain") %>%
        add_categories(ds_model_fits[[1]], c("species", "year")),
      pattern = map(ds_model_fits),
      iteration = "list"
    ),
    # Select model with lowest AIC, within tolerance with lowest nr. of params
    tar_target(
      name = model_selection,
      command = select_models(
        aic_diff = aic_comparison,
        model_list = ds_model_fits,
        aic_tol = 2
      ),
      pattern = map(aic_comparison, ds_model_fits),
      iteration = "list"
    ),

    ## Get distance sampling results
    # Detection probabilities
    tar_target(
      name = detection_probabilities,
      command = get_det_probs(ds_model = model_selection) %>%
        add_categories(model_selection, c("species", "year")),
      pattern = map(model_selection),
      iteration = "list"
    ),
    # Abundances
    tar_target(
      name = abundances_stratum,
      command = get_individuals_from_ds(
        ds_model = model_selection,
        measure = "abundance"
      ) %>%
        filter(
          !grepl("^Weidestreek", Label),
          Label != "Total"
        ) %>%
        add_categories(model_selection, c("species", "year")),
      pattern = map(model_selection),
      iteration = "list"
    ),
    # Densities
    tar_target(
      name = densities_stratum,
      command = get_individuals_from_ds(
        ds_model = model_selection,
        measure = "dens"
      ) %>%
        filter(
          !grepl("^Weidestreek", Label),
          Label != "Total"
        ) %>%
        add_categories(model_selection, c("species", "year")),
      pattern = map(model_selection),
      iteration = "list"
    )
  )
)
