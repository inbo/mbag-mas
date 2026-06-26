# Load packages
library(targets)
library(magick)
library(ggplot2)
library(INBOtheme)

theme_set(theme_inbo(base_size = 12, transparent = TRUE))
conflicted::conflicts_prefer(dplyr::filter)

source("R/tar_read_mas.R")
mbag_dir <- rprojroot::find_root_file(criterion = rprojroot::is_git_root)
targets_path <- file.path(mbag_dir, "source", "targets", "power_mas_light")

# Get input for power calculation
design_list <- tar_read_mas("design_list", branch = 1)[[1]]
source(file.path(targets_path, "R", "simulate_mas_data.R"))

# Capture plot
p <- grDevices::recordPlot({
  designpower::find_power(
    design = design_list$design[
      -which(names(design_list$design) == "tar_group")
    ],
    design_digits = design_list$digits,
    opti = "beta_3",
    sim_power = simulate_mas_data,
    power = 0.9,
    alpha = 0.1,
    filename = file.path(targets_path, "power_mas_light_akkervogel2.duckdb")
  )
})

# Save recorded plot
png("./figures/cover_raw.png", width = 800, height = 400)
p
dev.off()

# Cut image
img <- image_read("./figures/cover_raw.png")
info <- image_info(img)

# Parameters
cut_start <- 70    # x-coordinate where the cut starts
cut_width <- 125   # width of the strip to remove

# Left part
left <- image_crop(
  img,
  geometry = sprintf("%dx%d+0+0",
                     cut_start,
                     info$height)
)

# Right part
right <- image_crop(
  img,
  geometry = sprintf("%dx%d+%d+0",
                     info$width - cut_start - cut_width,
                     info$height,
                     cut_start + cut_width)
)

# Stitch them together horizontally
merged <- image_append(c(left, right), stack = FALSE)

# Crop 100 pixels from the right
info_merged <- image_info(merged)
cropped <- image_crop(
  merged,
  geometry = sprintf("%dx%d+0+0", info_merged$width - 90, info_merged$height)
)

# Save final cover image
image_write(cropped, "./figures/cover.png")
