# post_render_cleanup.R
# This script cleans up 09_densiteitsschattingen.qmd after rendering.
# It removes everything after "## Resultaten" and adds an informative message.

main_qmd <- "09_densiteitsschattingen.qmd"

# Read file
lines <- readLines(main_qmd)

# Find "## Resultaten"
res_line <- grep("^## Resultaten", lines)
if (length(res_line) == 0) stop("## Resultaten not found in QMD")

# Keep everything up to ## Resultaten
lines_new <- lines[1:res_line]

# Add informative message
msg <- c(
  "",
  "> ⚠️ **Let op:** de inhoud onder `## Resultaten` wordt automatisch",
  "> gegenereerd tijdens het renderen via `render_children.R`.",
  "> Pas dit deel niet manueel aan."
)

lines_new <- c(lines_new, msg)

# Overwrite the file
writeLines(lines_new, main_qmd)


# Flanders qmd post render
flandersqmd::post_render()
