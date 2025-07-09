tar_read_sf <- function(name, store) {
  # Get target data
  list_sf <- targets::tar_read_raw(name = name, store = store)

  # Create dataframe
  out_df <- do.call(
    what = rbind.data.frame,
    args = c(list_sf, make.row.names = FALSE)
  )

  return(out_df)
}
