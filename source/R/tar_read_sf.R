tar_read_sf <- function(name, store) {
  do.call(rbind.data.frame,
          tar_read_raw(name = name, store = store))
}
