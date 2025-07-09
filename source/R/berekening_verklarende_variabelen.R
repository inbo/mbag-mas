# Proportie beheerovereenkomst per telcirkel per jaar
add_bo_by_year <- function(punten_df, year_var = "jaar", var_col, ...) {
  require("dplyr")
  require("rlang")
  require("sf")
  require("lubridate")

  # Read in bo layer and calculate start and stop year
  ## Get right year for loading data
  year <- pull(distinct(st_drop_geometry(punten_df[year_var])))
  if (year >= 2018 && year <= 2022) {
    bo_file_year <- 2022
  } else if (year == 2023) {
    bo_file_year <- 2023
  } else if (year >= 2024 && year <= 2028) {
    bo_file_year <- 2024
  } else {
    warning("No calculation of bo prop. taken for years outside 2018-2028.")
    out_df_year <- punten_df %>%
      mutate(!!var_col := NA)

    return(out_df_year)
  }

  ## Read data
  path_bo <- path_to_bo(jaar = bo_file_year)
  bo_layer <- sf::st_read(dsn = path_bo, quiet = TRUE) %>%
    sf::st_transform(crs = 31370)
  if (bo_file_year == 2024) {
    bo_layer <- bo_layer %>%
      rename("BH_DOELST" = "BHDOELST")
  }

  # Calculate start and stop year variables
  bo_layer_filtered <- bo_layer %>%
    mutate(startjaar = year(.data$START),
           stopjaar = year(.data$STOP)) %>%
    filter(.data$startjaar <= year & .data$stopjaar >= year)

  # Calculate percentage of bo for each point location
  out_df_year <- add_bo_to_frame(punten_df = punten_df,
                                 bo_layer = bo_layer_filtered,
                                 ...) %>%
    rename(!!var_col := "area_prop_sb")

  return(out_df_year)
}

read_crop_layers_by_year <- function(
    path_to_crop_layer,
    cut_bo = TRUE) {
  require("dplyr")
  require("lubridate")
  require("sf")
  require("qgisprocess")

  # Read crop layer
  crop_layer <- sf::st_read(path_to_crop_layer, quiet = TRUE) %>%
    sf::st_transform(crs = 31370)
  if (!"geometry" %in% names(crop_layer)) {
    crop_layer <- sf::st_set_geometry(crop_layer, "geometry")
  }

  # Get year
  year <- unique(as.numeric(
    stringr::str_extract_all(path_to_crop_layer, "[0-9]+")[[1]]
  ))
  crop_layer$jaar <- year

  if (cut_bo) {
    if (year >= 2018 && year <= 2022) {
      bo_file_year <- 2022
    } else if (year == 2023) {
      bo_file_year <- 2023
    } else if (year >= 2024 && year <= 2028) {
      bo_file_year <- 2024
    } else {
      warning("No difference with bo layer taken for years outside 2018-2028.")
      return(crop_layer)
    }

    ## Read data
    path_bo <- path_to_bo(jaar = bo_file_year)
    bo_layer <- sf::st_read(dsn = path_bo, quiet = TRUE) %>%
      sf::st_transform(crs = 31370)
    if (bo_file_year == 2024) {
      bo_layer <- bo_layer %>%
        rename("BH_DOELST" = "BHDOELST")
    }

    # Calculate start and stop year variables
    bo_layer_filtered <- bo_layer %>%
      mutate(startjaar = year(.data$START),
             stopjaar = year(.data$STOP)) %>%
      filter(.data$startjaar <= year & .data$stopjaar >= year) %>%
      st_buffer(0.1)

    # Take intersection with crop layer
    crop_layer_intersect <- crop_layer %>%
      qgisprocess::qgis_run_algorithm_p(
        algorithm = "native:difference",
        OVERLAY = bo_layer_filtered,
        OUTPUT = qgisprocess::qgis_tmp_vector()
      ) %>%
      qgisprocess::qgis_extract_output("OUTPUT") %>%
      sf::st_as_sf()
    crop_layer_intersect <- sf::st_set_geometry(crop_layer_intersect,
                                                "geometry")

    return(crop_layer_intersect)
  } else {
    return(crop_layer)
  }
}

calc_crop_prop_by_year <- function(punten_df, crop_layer, group_by_col) {
  require("dplyr")
  require("sf")

  points_vzml <- landusemetrics_grid_cell(
    grid_cell = punten_df %>%
      st_buffer(dist = 300),
    layer = crop_layer  %>%
      group_by(.data$geometry) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      mutate(weight = 1 / n),
    grid_group_by_col = "pointid",
    layer_group_by_col = group_by_col,
    weight_col = "weight"
  )

  points_vzml <- points_vzml %>%
    ungroup()

  return(points_vzml)
}

# Proportie hoofdteelten per telcirkel per jaar
calc_lbg_by_year <- function(punten_df) {
  # Loop over years
  years <- unique(punten_df$jaar)
  out_list <- vector(mode = "list", length = length(years))

  for (i in seq_along(years)) {
    year <- years[i]

    # Filter by year
    punten_df_year <- punten_df %>% filter(.data$jaar == year)
    lbg_file_year <- path_to_lbg(jaar = year)

    out_df_year <- calc_lbg(path = lbg_file_year,
                            punten_sf = punten_df_year)

    out_list[[i]] <- out_df_year %>% ungroup() %>% mutate(jaar = year)
  }

  return(do.call(rbind.data.frame, out_list))
}

# Binnen buiten sbp per regio
add_sbp_per_regio <- function(punten_sf, perimeters) {

  out_list <- vector(mode = "list", length = length(perimeters$Naam))
  i <- 0

  for (r in perimeters$Naam) {
    i <- i + 1

    # Filter by region
    punten_df_regio <- punten_sf %>% filter(.data$regio == r)
    sbp_akkervogels_regio <- read_sbp_akkervogels(
      path = path_to_sbp_akkervogels(),
      gebied = perimeters %>% filter(.data$Naam == r)
    )

    telpunten_2018_2022_regio <- add_stratum_sbp(
      punten_sf = punten_df_regio,
      sbp = sbp_akkervogels_regio
    ) %>%
      mutate(sbp = ifelse(.data$is_sbp == TRUE, "binnen", "buiten"))

    out_list[[i]] <- telpunten_2018_2022_regio
  }

  return(do.call(rbind.data.frame, out_list))
}

# Perceelgroottes per telcirkel per jaar
calc_perceelsgrootte_by_year <- function(punten_df) {
  # Loop over years
  years <- unique(punten_df$jaar)
  out_list <- vector(mode = "list", length = length(years))

  for (i in seq_along(years)) {
    year <- years[i]
    number_string <- sub(".*([0-9]{2})$", "\\1", year)

    # Filter by year
    punten_df_year <- punten_df %>% filter(.data$jaar == year)
    lbg_binding <- st_read(file.path("data", "landbouwgebruikspercelen",
                                     "Shapefile", paste0("Lbgbrprc",
                                                         number_string,
                                                         ".shp"))) %>%
      st_transform(crs = 31370)

    if (year %in% 2022:2023) {
      lbg_binding <- lbg_binding %>%
        rename(LBLHFDTLT = GWSNAM_H,
               perc_id = REF_ID)
    } else {
      lbg_binding <- lbg_binding %>%
        rename(perc_id = OIDN)
    }

    # Intersection
    intersect <- st_intersection(punten_df_year %>% st_buffer(300), lbg_binding)

    out_df_year <- lbg_binding %>%
      filter(perc_id %in% intersect$perc_id) %>%       # Filter percelen
      full_join(st_drop_geometry(intersect)) %>%
      filter(!is.na(LBLHFDTLT)) %>%

      # Join neighbouring polygons by main crop
      group_by(pointid, LBLHFDTLT) %>%
      summarise() %>%
      st_cast("MULTIPOLYGON") %>%
      st_cast("POLYGON") %>%
      ungroup() %>%

      # Calculate median perceel area by point
      mutate(area = st_area(geometry)) %>%
      st_drop_geometry() %>%
      group_by(pointid) %>%
      summarise(perceel_median_area = units::drop_units(median(area)),
                perceel_iqr_area = IQR(area),
                perceel_cv_area = perceel_iqr_area / perceel_median_area,
                .groups = "drop")

    out_list[[i]] <- out_df_year %>% full_join(punten_df_year)
  }

  return(do.call(rbind.data.frame, out_list))
}

# Voeg inertia parameter toe
add_bo_inertia <- function(punten_df, path_bo, bh_doel) {
  # Read in bo layer and calculate start and stop year
  bo_layer <- read_bo(path = path_bo)
  bo_layer2 <- bo_layer %>%
    mutate(startjaar = year(START),
           stopjaar = year(STOP))

  # Loop over years
  years <- unique(punten_df$jaar)
  out_list <- vector(mode = "list", length = length(years))

  for (i in seq_along(years)) {
    year <- years[i]

    # Filter by year
    punten_df_year <- punten_df %>% filter(jaar == year)
    bo_layer_year <- bo_layer2 %>%
      filter(startjaar <= year & stopjaar >= year)

    out_df_year <- st_intersection(st_buffer(punten_df_year, 300),
                                   bo_layer_year) %>%
      st_drop_geometry() %>%
      filter(BH_DOELST %in% bh_doel) %>%
      mutate(inertia_row = jaar - startjaar + 1) %>%
      group_by(pointid) %>%
      summarise(inertia = round(mean(.data$inertia_row)), .groups = "drop")

    out_list[[i]] <- out_df_year %>%
      full_join(punten_df_year, by = c("pointid")) %>%
      replace(is.na(.), 0) %>%
      mutate(jaar = year)
  }

  return(do.call(rbind.data.frame, out_list))
}

# Selecteer (multi)polygonen binnen buffered perimeter (ev. per gebied)
buffer_layers_to_perimeter <- function(layer, buffer_km, group_var = NULL) {
  buffer_to_meters <- buffer_km * 1000

  if (is.null(group_var)) {
    layer <- layer %>%
      tibble::rownames_to_column("id")

    polygons <- perimeters %>%
      st_buffer(buffer_to_meters) %>%
      st_intersection(layer) %>%
      pull(id)

    out <- layer %>%
      filter(id %in% polygons) %>%
      select(-id)
  } else {
    polygons <- perimeters %>%
      st_buffer(buffer_to_meters) %>%
      st_intersection(layer) %>%
      pull(group_var)

    out <- layer %>%
      filter(!!sym(group_var) %in% polygons)
  }
  return(out)
}

# Opsplitsen in aparte polygonen (ev. mergen per gebied eerst)
splits_polys <- function(layer, id_name, group_var = NULL) {
  if (is.null(group_var)) {
    out <- layer %>%
      st_cast("POLYGON") %>%
      tibble::rownames_to_column("id") %>%
      mutate(id = paste(id_name, id, sep = "_")) %>%
      select(id)
  } else {
    out <- layer %>%
      group_by_at(group_var) %>%
      summarise() %>%
      st_cast("MULTIPOLYGON") %>%
      st_cast("POLYGON") %>%
      tibble::rownames_to_column("id") %>%
      mutate(id = paste(id_name, id, sep = "_")) %>%
      select(id)
  }
  return(out)
}

# visualizeWeights function of GVI for R 4.3.0
visualize_weights <- function(x, m = 0.5, b = 8,
                              mode = c("logit", "exponential")) {
  if (is(x, "SpatRaster")) {
    xy <- x %>% terra::xyFromCell(which(x[] == 1))
    max_dist <- (terra::nrow(x) / 2) * terra::res(x)[1]
  } else if (is.numeric(x)) {
    max_dist <- x
  } else {
    stop("x needs to be numeric or a SpatRaster object")
  }
  if (all(mode == c("logit", "exponential")) || mode == "logit") {
    logfun <- function(x) {
      return(1 / (1 + exp((b) * (x - m))))
    }
    plot_main <- paste0("Mode: logit\nm: ", m, "    b: ",
                        b)
  } else if (mode == "exponential") {
    logfun <- function(x) {
      return(1 / (1 + ((b) * x^(m))))
    }
    plot_main <- paste0("Mode: exponential\nm: ", m, "    b: ",
                        b)
  } else {
    stop("Currently only logit and exponential are supported")
  }
  plot(logfun(seq(0, 1, length.out = max_dist)), type = "l",
       ylab = "Decay Weight (d)", xlab = "Distance [m]", main = plot_main)
}

# Calculate weights for exponential and logistic decay functions
calc_weights <- function(x, max_dist, m = NULL, b = NULL, mode = "logistic") {
  if (mode == "logistic") {
    # Parameters
    if (is.null(m)) {
      m <- 0.5
    }
    if (is.null(b)) {
      b <- 8
    }

    # Rescale
    m <- m * max_dist
    b <- b / max_dist

    # Function
    out <- 1 / (1 + exp((b) * (x - m)))

  } else if (mode == "exponential") {
    # Parameters
    if (is.null(m)) {
      m <- 1
    }
    if (is.null(b)) {
      b <- 6
    }

    # Rescale
    b <- b / max_dist

    # Function
    out <- 1 / (1 + ((b) * x^(m)))

  } else {
    stop("Currently only logistic and exponential are supported", call. = FALSE)
  }

  return(out)
}
