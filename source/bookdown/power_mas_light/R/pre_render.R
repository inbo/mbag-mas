library(flandersqmd)

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

# Create child document for each species
# Include child files in report
autoqmd_prepare(
  species = target_species,
  label = gsub("\\s", ".", tolower(target_species)),
  branch = seq_along(target_species),
  template = "_simulatieparameters_per_soort.qmd",
  child_dir = "child_qmd",
  target_file = "04_schatting_simulatieparameters.qmd"
)
