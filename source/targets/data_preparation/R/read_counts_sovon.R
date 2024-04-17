# Read in raw data from SOVON
read_counts_sovon <- function(proj_path, file) {
  file_path <- file.path(proj_path, "data", "mas", file)
  return(read_sf(file_path))
}
