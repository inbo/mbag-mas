# Load functions
library(dplyr)

mbag_dir <- rprojroot::find_root_file(criterion = rprojroot::is_git_root)
tar_f <- file.path(mbag_dir, "source", "targets", "power_mas_light", "R")

source(file.path(tar_f, "simulate_mas_data.R"))
source(file.path(tar_f, "detectable_effect_to_df.R"))
source("R/tar_read_mas.R")

# Get design of first scenario
design_list <- tar_read_mas("design_list")
design <- design_list[[1]]$design[
  -which(names(design_list[[1]]$design) == "tar_group")
]
design_digits <- design_list[[1]]$digits

# Replace values for sigma_punt
parameters_df <- tar_read_mas("parameters_df")
mean_sigma <- mean(parameters_df$sigma_punt)

sigma_effect <- (exp(diff(qnorm(c(0.025, 0.975), sd = mean_sigma))) - 1) * 100
paste("Het verschil tussen een telpunt aan de onderkant en de bovenkant van de",
      "verdeling van telpunteffecten bedraagt", round(sigma_effect, 1), "%.")

design$sigma_punt <- mean_sigma
design_digits["sigma_punt"] <- 2

# Run power analysis
set.seed(123)

high_sigma_power <- designpower::find_power(
  design = design,
  design_digits = design_digits,
  opti = "beta_3",
  sim_power = simulate_mas_data,
  power = 0.9,
  alpha = 0.1,
  filename = "R/test_power_higher_sigma.duckdb"
)


data.frame(
  beta3 = high_sigma_power[1]
) %>%
  mutate(
    effect_pct = 100 * (exp(.data$beta3) - 1),
    effect_pct_10years = 100 * (exp(.data$beta3 * 10) - 1)
  )

detectable_effect_df <- tar_read_mas("detectable_effect_df")
detectable_effect_df %>%
  filter(n_telpunten == 100, n_jaar == 10) %>%
  select("beta3", "effect_pct", "effect_pct_10years" = "effect_pct_long")
