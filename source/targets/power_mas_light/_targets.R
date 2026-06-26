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
      filter(broedcode > 0 | naam == "Torenvalk") %>%
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
  tar_target(
    name = presence_logical,
    command = target_sp_pa %>%
      mutate(regio = ifelse(grepl("\\sleemstreek", regio),
                            "Leemstreek", regio)) %>%
      summarise(
        total = sum(count),
        .by = c("plotnaam", "openheid_klasse", "sbp", "regio")
      ) %>%
      mutate(present = total > 0),
    pattern = map(target_sp_pa)
  ),
  tar_target(
    name = presence_prop,
    command = presence_logical %>%
      count(present) %>%
      mutate(prop = n / sum(n)) %>%
      filter(present) %>%
      pull(prop),
    pattern = map(presence_logical)
  ),

  ## Estimate parameters
  # Filter out zeroes
  tar_target(
    name = target_sp_pa_nozero,
    command = target_sp_pa %>%
      left_join(presence_logical,
                by = join_by(plotnaam, regio, openheid_klasse, sbp)) %>%
      filter(present) %>%
      select(-"present"),
    pattern = map(target_sp_pa, presence_logical)
  ),
  # Prepare model data
  tar_target(
    name = model_data,
    command = target_sp_pa_nozero %>%
      filter(period_count == best_period) %>%
      mutate(sbp_f = factor(sbp, levels = c("buiten", "binnen")),
             plotnaam_f = factor(plotnaam),
             year2 = year - 2024),
    pattern = map(target_sp_pa_nozero, best_period)
  ),
  tar_target(
    name = species,
    command = unique(model_data$naam),
    pattern = map(model_data)
  ),

  # Fit model
  tar_target(
    name = fit_model,
    command = list(
      naam = species,
      fit = glmmTMB::glmmTMB(
        count ~ sbp_f + (1 | plotnaam_f),
        data = model_data,
        family = poisson()
      )
    ),
    pattern = map(species, model_data),
    iteration = "list"
  ),

  # Model diagnostics
  tar_target(
    name = simulated_residuals,
    command = DHARMa::simulateResiduals(fittedModel = fit_model$fit),
    pattern = map(fit_model),
    iteration = "list"
  ),

  # Extract parameters
  tar_target(
    name = parameters,
    command = extract_parameters(fit_model$fit),
    pattern = map(fit_model),
    iteration = "list"
  ),
  tar_target(
    name = parameters_df_grouped,
    command = data.frame(naam = species, as.data.frame(parameters),
                         prop = presence_prop),
    pattern = map(species, parameters, presence_prop)
  ),
  tar_target(
    name = parameters_df,
    command = bind_rows(parameters_df_grouped) %>%
      filter(prop >= 0.2),
  ),
  tar_target(
    name = best_parameters,
    command = parameters_df %>%
      summarise(
        beta_0 = max(parameters_df$beta_0),
        beta_2 = max(parameters_df$beta_2),
        sigma_punt = min(parameters_df$sigma_punt)
      )
  ),

  ## Power analysis
  # Prepare scenarios
  tar_group_size(
    name = scenarios,
    command = expand.grid(
      n_telpunten = c(100, 200, 400),
      n_jaar = 10,
      beta_1 = c(log(0.99), log(1), log(1.01))
    ) %>%
      bind_rows(
        expand.grid(
          n_telpunten = c(100, 200, 400),
          n_jaar = 16,
          beta_1 = log(0.99)
        )
      ) %>%
      bind_rows(
        expand.grid(
          n_telpunten = c(100, 200, 400),
          n_jaar = 24,
          beta_1 = log(0.99)
        )
      ) %>%
      tidyr::crossing(best_parameters) %>%
      rowwise() %>%
      mutate(
        beta_3 = ifelse(
          n_jaar == 24,
          log(1.005),
          trend_to_beta_param(
            trend_buiten = exp(beta_1)
          )
        )
      ) %>%
      ungroup() %>%
      arrange(n_jaar, beta_1, n_telpunten),
    size = 1
  ),
  # Prepare design lists
  tar_target(
    name = design_list,
    command = prepare_design(
      scenarios,
      digits = c(
        n_jaar = 0,
        n_telpunten = 0,
        beta_0 = 2,
        beta_1 = 2,
        beta_2 = 2,
        beta_3 = ifelse(scenarios$n_jaar == 24, 4, 3),
        sigma_punt = 6
      )
    ),
    pattern = map(scenarios),
    iteration = "list"
  ),

  # Run simulations
  tar_target(
    name = detectable_effect,
    command = custom_find_power(
      design = design_list$design,
      design_digits = design_list$digits,
      opti = "beta_3",
      sim_power = simulate_mas_data,
      power = 0.9,
      alpha = 0.1,
      db_file = "power_mas_light_akkervogel2.duckdb",
      seed = 123
    ),
    pattern = map(design_list),
    iteration = "list"
  ),

  # Get results
  tar_target(
    name = detectable_effect_df,
    command = detectable_effect_to_df(detectable_effect)
  )
)
