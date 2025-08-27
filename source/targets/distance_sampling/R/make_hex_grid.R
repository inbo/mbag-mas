make_hex_grid <- function(x, area = 300 * 300 * pi) {
  require("sf")

  # target_area can be numeric (m^2) or units
  if (!inherits(area, "units")) {
    target_area <- units::set_units(area, "m^2")
  }

  # build grid
  grid <- st_make_grid(
    x,
    cellsize = target_area,
    square = FALSE
  )
  grid <- st_sf(geometry = grid)

  # get centroids of hexagons
  centroids <- st_centroid(grid)

  # find which centroids fall inside x
  inside_idx <- lengths(st_within(centroids, x)) > 0

  # keep only hexagons with centroids inside
  grid <- grid[inside_idx, ]

  # extract attributes from x
  attributes <- st_join(centroids, x, join = st_intersects)
  attributes_df <- st_drop_geometry(attributes[inside_idx, ])

  out_grid <- cbind(grid, attributes_df)

  return(out_grid)
}
