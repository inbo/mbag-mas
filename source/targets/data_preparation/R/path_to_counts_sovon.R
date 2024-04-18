# Read in raw data from SOVON
path_to_counts_sovon <- function(proj_path, file) {
  file_path <- file.path(proj_path, "data", "mas", file)
  return(file_path)
}
