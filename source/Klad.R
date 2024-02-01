oude_circkels <- st_read(here(
  "data",
  "SOVON",
  "avimap_601_0_MAS_Vlaanderen_telpunten_xy.shp"
)) %>%
  st_transform(crs = 31370)

oude_circkels <- oude_circkels %>%
  filter(st_within(oude_circkels, perimeters_data, sparse = FALSE))

oude_circkels <- oude_circkels %>%
  mutate(sbp = ifelse(st_within(oude_circkels, sbp_akkervogels),
                      "binnen",
                      "buiten"))

ol <- selectie_openheid(
  gebied = perimeters_data,
  ol_strata = c("OL"))

hol <- selectie_openheid(
  gebied = perimeters_data,
  ol_strata = c("HOL"))

oude_circkels <- oude_circkels %>%
  mutate(openheid_klasse = ifelse(st_within(oude_circkels, hol),
                                  "HOL",
                                  NA))

oude_circkels <- oude_circkels %>%
  mutate(openheid_klasse = ifelse(st_within(oude_circkels, ol),
                                  "OL",
                                  openheid_klasse))



















install.packages("osmdata")

library(osmdata)

coordinates <- st_coordinates(st_transform(steekproefkader_finaal, crs = 4326))

points_df <- data.frame(
  lat = coordinates[, 2],
  lon = coordinates[, 1]
)

# Create a bounding box around the points
bbox <- matrix(c(min(points_df$lon), min(points_df$lat), max(points_df$lon), max(points_df$lat)), ncol = 2)

# Fetch OSM data using the bounding box
osm_query <- opq(bbox) %>%
  osmdata_sf()

# Extract the OSM object types for the points
osm_types <- osm_query$osm_points$osm_id$type
