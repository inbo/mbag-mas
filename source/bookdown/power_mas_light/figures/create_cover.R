# Load packages
library(targets)
library(magick)
library(ggplot2)
library(INBOtheme)
library(grid)
library(ragg)

theme_set(theme_inbo(base_size = 12, transparent = TRUE))
conflicted::conflicts_prefer(dplyr::filter)

options(OutDec = ",")

source("R/tar_read_mas.R")
mbag_dir <- rprojroot::find_root_file(criterion = rprojroot::is_git_root)
targets_path <- file.path(mbag_dir, "source", "targets", "power_mas_light")

# Get input for power calculation
design_list <- tar_read_mas("design_list", branch = 1)[[1]]
source(file.path(targets_path, "R", "simulate_mas_data.R"))

# ------------------------------------------------------------------
# Resolution scaling factor
# ------------------------------------------------------------------

scale <- 4

# ------------------------------------------------------------------
# Capture plot
# ------------------------------------------------------------------
designpower::find_power(
  design = design_list$design[
    -which(names(design_list$design) == "tar_group")
  ],
  design_digits = design_list$digits,
  opti = "beta_3",
  sim_power = simulate_mas_data,
  power = 0.9,
  alpha = 0.1,
  filename = file.path(
    targets_path,
    "power_mas_light_akkervogel2.duckdb"
  )
)

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
    filename = file.path(
      targets_path,
      "power_mas_light_akkervogel2.duckdb"
    )
  )
})

# ------------------------------------------------------------------
# Save at high resolution
# ------------------------------------------------------------------

agg_png(
  "./figures/cover_raw.png",
  width = 800 * scale,
  height = 400 * scale,
  units = "px",
  res = 96 * scale,
  background = "white"
)

p
dev.off()

# ------------------------------------------------------------------
# Remove middle strip
# ------------------------------------------------------------------

img <- image_read("./figures/cover_raw.png")
info <- image_info(img)

cut_start <- 88 * scale
cut_width <- 128 * scale

left <- image_crop(
  img,
  geometry = sprintf(
    "%dx%d+0+0",
    cut_start,
    info$height
  )
)

right <- image_crop(
  img,
  geometry = sprintf(
    "%dx%d+%d+0",
    info$width - cut_start - cut_width,
    info$height,
    cut_start + cut_width
  )
)

merged <- image_append(c(left, right), stack = FALSE)

info_merged <- image_info(merged)

# ------------------------------------------------------------------
# Remove legend right
# ------------------------------------------------------------------

cropped <- image_crop(
  merged,
  geometry = sprintf(
    "%dx%d+0+0",
    info_merged$width - 120 * scale,
    info_merged$height
  )
)

image_write(cropped, "./figures/cover_en.png")

# ------------------------------------------------------------------
# Replace axis titles
# ------------------------------------------------------------------

img <- image_read("./figures/cover_en.png")
img <- image_draw(img)

rect(
  xleft = 5 * scale,
  ybottom = 80 * scale,
  xright = 30 * scale,
  ytop = 320 * scale,
  col = "white",
  border = NA
)

grid.text(
  "Geschatte power",
  x = unit(16 * scale, "pt"),
  y = unit(0.53, "npc"),
  rot = 90,
  gp = gpar(
    fontsize = 15 * scale,
    fontfamily = "sans",
    col = "black"
  )
)

rect(
  xleft = 200 * scale,
  xright = 300 * scale,
  ybottom = 370 * scale,
  ytop = 392 * scale,
  col = "white",
  border = NA
)

grid.text(
  expression(beta[3] ~ "(effectgrootte)"),
  x = unit(0.55, "npc"),
  y = unit(0.05, "npc"),
  gp = gpar(
    fontsize = 15 * scale,
    fontfamily = "sans",
    col = "black"
  )
)

dev.off()

image_write(img, "./figures/cover.png")
