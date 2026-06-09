################################################################################
# targets pipeline for power analysis MAS light                                #
# see https://books.ropensci.org/targets/                                      #
################################################################################

# Load packages required to define the pipeline
library(targets)
library(tarchetypes)
library(dplyr)
library(readr)
library(glmmTMB)

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
  ## Prepare data
  # Read occurrences
  tar_file(
    name = mas_data_clean_file,
    command = file.path(mbag_dir, "output", "datasets", "mas_data_clean.csv")
  ),
  tar_target(
    name = mas_data_target_sp_raw,
    command = read_csv(mas_data_clean_file, show_col_types = FALSE) %>%
      filter(tolower(naam) %in% tolower(target_species))
  ),
  # Prepare occurrences and group by species (dynamic)
  tar_group_by(
    name = mas_data_target_sp,
    command = mas_data_target_sp_raw %>%
      select("plotid", "plotnaam", "naam", "aantal", "jaar",
             "broedcode" = "wrntype", "regio", "openheid_klasse", "sbp",
             "periode_in_jaar") %>%
      # Only breeding individuals and count by site
      filter(broedcode > 0) %>%
      group_by(plotid, plotnaam, naam, jaar, regio, openheid_klasse, sbp,
               periode_in_jaar) %>%
      summarise(count = sum(.data$aantal), .groups = "drop") %>%
      # Select data from 2024 onwards
      filter(jaar %in% 2024:2025),
    naam
  ),

  # Read visits
  tar_file(
    name = visits_file,
    command = file.path(mbag_dir, "data", "steekproefkaders",
                        "bezoekenlijst_2023_2025.csv")
  ),
  tar_target(
    name = visits,
    command = read_csv(visits_file, show_col_types = FALSE) %>%
      filter(year %in% 2024:2025)
  ),

  # Add absences (dynamic branching by species)
  tar_target(
    name = target_sp_pa,
    command = add_absences(mas_data_target_sp, visits) %>%
      group_by(naam) %>%
      tar_group(),
    pattern = map(mas_data_target_sp)
  ),

  # Get best counting periods
  tar_target(
    name = best_period,
    command = target_sp_pa %>%
      summarise(sum = sum(count), .by = c(year, period_count)) %>%
      summarise(mean_sum = mean(sum), .by = period_count) %>%
      slice_max(mean_sum) %>%
      pull(period_count),
    pattern = map(target_sp_pa)
  ),

  ## Estimate parameters
  # Prepare data
  tar_target(
    name = model_data,
    command = target_sp_pa %>%
      filter(period_count == best_period) %>%
      mutate(sbp_f = factor(sbp, levels = c("buiten", "binnen")),
             plotnaam_f = factor(plotnaam),
             year2 = year - 2024),
    pattern = map(target_sp_pa, best_period)
  ),
  tar_target(
    name = species,
    command = unique(model_data$naam),
    pattern = map(model_data)
  ),

  # Fit models
  tar_target(
    name = fit_models,
    command = list(
      poisson = glmmTMB::glmmTMB(
        count ~ year2 * sbp_f + (1 | plotnaam_f),
        data = model_data,
        family = poisson()
      ),
      negbin =  glmmTMB::glmmTMB(
        count ~ year2 * sbp_f + (1 | plotnaam_f),
        data = model_data,
        family = nbinom2()
      )
    ),
    pattern = map(model_data),
    iteration = "list"
  ),

  # Select model
  tar_target(
    name = final_model,
    command = c(naam = species, select_model(fit_models)),
    pattern = map(species, fit_models),
    iteration = "list"
  ),

  # Extract parameters
  tar_target(
    name = parameters,
    command = extract_parameters(final_model$fit),
    pattern = map(final_model),
    iteration = "list"
  ),
  tar_target(
    name = parameters_df_grouped,
    command = data.frame(naam = species, as.data.frame(parameters)),
    pattern = map(species, parameters)
  ),
  tar_target(
    name = parameters_df,
    command = bind_rows(parameters_df_grouped),
  ),

  ## Power analysis
  tar_target(
    name = scenarios,
    command = expand.grid(
      n_telpunten = c(100, 200),
      n_jaar = 10,
      beta_1 = c(log(0.99)),
      beta_3 = c(-log(0.99))
    )
  ),
  tar_target(
    name = species_scenarios,
    command = tidyr::crossing(
      parameters_df %>%
        select("naam", "beta_0", "beta_2", "sigma_punt", "theta"),
      scenarios
    )
  ),

  tar_map(
    values = list(
      species = target_species[1:2]
    ),

    # Go over each row
    tar_group_size(
      name = species_scenarios_clean,
      command = species_scenarios %>%
        filter(naam == species) %>%
        select(-"naam"),
      size = 1
    ),

    # Prepare design lists
    tar_target(
      name = design_list,
      command = prepare_design(
        species_scenarios_clean,
        digits = c(
          n_jaar = 0,
          n_telpunten = 0,
          beta_0 = 2,
          beta_1 = 2,
          beta_2 = 2,
          beta_3 = 3,
          sigma_punt = 2,
          theta = 2
        )
      ),
      pattern = map(species_scenarios_clean),
      iteration = "list"
    ),

    tar_target(
      name = detectable_effect,
      command = designpower::find_power(
        design = design_list$design[
          -which(names(design_list$design) == "tar_group")
        ],
        design_digits = design_list$digits,
        opti = "beta_3",
        sim_power = simulate_mas_data,
        power = 0.9,
        alpha = 0.1,
        filename = paste0("power_mas_light_",
                          gsub("\\s", ".", tolower(species)),
                          ".duckdb")
      ),
      pattern = map(design_list),
      iteration = "list"
    )
  )
)
