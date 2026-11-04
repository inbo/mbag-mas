# Load functions
library(dplyr)
library(ggplot2)

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
) |>
  dplyr::mutate(
    effect_pct = 100 * (exp(.data$beta3) - 1),
    effect_pct_10years = 100 * (exp(.data$beta3 * 10) - 1)
  )

detectable_effect_df <- tar_read_mas("detectable_effect_df")
detectable_effect_df |>
  dplyr::filter(n_telpunten == 100, n_jaar == 10) |>
  dplyr::select("beta3", "effect_pct", "effect_pct_10years" = "effect_pct_long")


# Test some more
design$sigma_punt <- 0.01
high_sigma_power2 <- designpower::find_power(
  design = design,
  design_digits = design_digits,
  opti = "beta_3",
  sim_power = simulate_mas_data,
  power = 0.9,
  alpha = 0.1,
  filename = "R/test_power_higher_sigma.duckdb"
)

design$sigma_punt <- 0.1
high_sigma_power3 <- designpower::find_power(
  design = design,
  design_digits = design_digits,
  opti = "beta_3",
  sim_power = simulate_mas_data,
  power = 0.9,
  alpha = 0.1,
  filename = "R/test_power_higher_sigma.duckdb"
)

design$sigma_punt <- 1
high_sigma_power4 <- designpower::find_power(
  design = design,
  design_digits = design_digits,
  opti = "beta_3",
  sim_power = simulate_mas_data,
  power = 0.9,
  alpha = 0.1,
  filename = "R/test_power_higher_sigma.duckdb"
)

design$sigma_punt <- 0.8
high_sigma_power5 <- designpower::find_power(
  design = design,
  design_digits = design_digits,
  opti = "beta_3",
  sim_power = simulate_mas_data,
  power = 0.9,
  alpha = 0.1,
  filename = "R/test_power_higher_sigma.duckdb"
)

design$sigma_punt <- 1.5
high_sigma_power6 <- designpower::find_power(
  design = design,
  design_digits = design_digits,
  opti = "beta_3",
  sim_power = simulate_mas_data,
  power = 0.9,
  alpha = 0.1,
  filename = "R/test_power_higher_sigma.duckdb"
)

design$sigma_punt <- 2
high_sigma_power7 <- designpower::find_power(
  design = design,
  design_digits = design_digits,
  opti = "beta_3",
  sim_power = simulate_mas_data,
  power = 0.9,
  alpha = 0.1,
  filename = "R/test_power_higher_sigma.duckdb"
)


powers_df <- dplyr::bind_rows(
  detectable_effect_df[1, ] |>
    dplyr::select("sigma_punt", "beta3", "effect_pct",
                  "effect_pct_10years" = "effect_pct_long"),
  data.frame(
    sigma_punt = mean_sigma,
    beta3 = high_sigma_power[1]
  ) |>
    dplyr::mutate(
      effect_pct = 100 * (exp(.data$beta3) - 1),
      effect_pct_10years = 100 * (exp(.data$beta3 * 10) - 1)
    ),
  data.frame(
    sigma_punt = 0.01,
    beta3 = high_sigma_power2[1]
  ) |>
    dplyr::mutate(
      effect_pct = 100 * (exp(.data$beta3) - 1),
      effect_pct_10years = 100 * (exp(.data$beta3 * 10) - 1)
    ),
  data.frame(
    sigma_punt = 0.1,
    beta3 = high_sigma_power3[1]
  ) |>
    dplyr::mutate(
      effect_pct = 100 * (exp(.data$beta3) - 1),
      effect_pct_10years = 100 * (exp(.data$beta3 * 10) - 1)
    ),
  data.frame(
    sigma_punt = 1,
    beta3 = high_sigma_power4[1]
  ) |>
    dplyr::mutate(
      effect_pct = 100 * (exp(.data$beta3) - 1),
      effect_pct_10years = 100 * (exp(.data$beta3 * 10) - 1)
    ),
  data.frame(
    sigma_punt = 0.8,
    beta3 = high_sigma_power5[1]
  ) |>
    dplyr::mutate(
      effect_pct = 100 * (exp(.data$beta3) - 1),
      effect_pct_10years = 100 * (exp(.data$beta3 * 10) - 1)
    ),
  data.frame(
    sigma_punt = 1.5,
    beta3 = high_sigma_power6[1]
  ) |>
    dplyr::mutate(
      effect_pct = 100 * (exp(.data$beta3) - 1),
      effect_pct_10years = 100 * (exp(.data$beta3 * 10) - 1)
    ),
  data.frame(
    sigma_punt = 2,
    beta3 = high_sigma_power7[1]
  ) |>
    dplyr::mutate(
      effect_pct = 100 * (exp(.data$beta3) - 1),
      effect_pct_10years = 100 * (exp(.data$beta3 * 10) - 1)
    )
) %>%
  arrange(sigma_punt)

powers_df


ggplot(powers_df, aes(x = sigma_punt, y = effect_pct_10years)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ poly(x, 2)")
