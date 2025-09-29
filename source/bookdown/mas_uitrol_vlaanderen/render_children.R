library(knitr)

# Globals
species <- sort(
  c(
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
)
main_qmd <- "09_densiteitsschattingen.qmd"

# -------------------------------
# 1. Define species and generate child QMDs
# -------------------------------
spec_dir <- "spec_files"
dir.create(file.path(spec_dir), showWarnings = FALSE)
output_files <- paste0("_", gsub("\\s", ".", tolower(species)), ".qmd")

for (i in seq_along(species)) {
  spec <- species[i]
  out_file <- output_files[i]
  knit_expand(
    "_species_densities.qmd", species = spec
  ) |>
    writeLines(file.path(spec_dir, out_file))
}

# -------------------------------
# 2. Modify main QMD
# -------------------------------
# Read the QMD
lines <- readLines(main_qmd)

# Find the line where ## Resultaten starts
res_line <- grep("^## Resultaten", lines)
if (length(res_line) == 0) stop("## Resultaten not found in QMD")

# Keep everything up to ## Resultaten
lines_new <- lines[1:res_line]

# Append the child includes as R chunk
child_lines <- character(0)
for (f in output_files) {
  file <- file.path(spec_dir, f)
  child_lines <- c(child_lines, sprintf("{{< include %s >}}\n", file))
}

# Combine
lines_new <- c(lines_new, "", child_lines)

# Remove trailing newline from the last line
lines_new[length(lines_new)] <- sub("\n$", "", lines_new[length(lines_new)])

# Write back to QMD
writeLines(lines_new, main_qmd)
