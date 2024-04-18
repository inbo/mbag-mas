# Transform Amersfoort coordinates to Lambert 72 correctly
# see https://inbo.github.io/tutorials/tutorials/spatial_transform_crs_2/
amersfoort_to_lambert72 <- function(sf_object) {
  sf_proj_network(TRUE)

  # Transform CRS according to specific pipeline
  pipelines <- sf_proj_pipelines("EPSG:28992", "EPSG:31370")

  # We select the pipeline with lowest accuracy, by filtering on accuracy
  chosen_pipeline_definition <- pipelines %>%
    slice_min(accuracy, n = 1) %>%
    pull(definition)

  # Transform according to most accurate pipeline
  out_sf <-
    st_transform(st_geometry(sf_object), "EPSG:31370",
                 pipeline = chosen_pipeline_definition
    ) %>%
    st_sf(st_drop_geometry(sf_object), geometry = .) %>%
    as_tibble() %>%
    st_as_sf() %>%
    rename(
      x_amersfoord = x_coord,
      y_amersfoord = y_coord
    ) %>%
    mutate(
      x_lambert = unlist(map(.data$geometry, 1)),
      y_lambert = unlist(map(.data$geometry, 2))
    ) %>%
    select(oid:y_amersfoord, x_lambert, y_lambert, everything())

  return(out_sf)
}

# Function to remove data counted twice at the same spot
process_double_counted_data <- function(counts_df) {
  # Professional bird counters
  profs <- c("WVNT00", "JJNN16", "NOVN00", "ETBX00")
  profs_2022 <- "RPLT02" # professional after 2022

  counts_df_state_pro <- counts_df %>%
    rename(waarnemer = waarneme) %>%
    mutate(status_teller = ifelse(
      (waarnemer %in% profs) | (waarnemer %in% profs_2022 & jaar >= 2022),
      "professioneel", "vrijwilliger"))

  # Identify points counted more than once per period
  doubles_df <- counts_df_state_pro %>%
    st_drop_geometry() %>%
    group_by(plotnaam, jaar, periode_in_jaar) %>%
    summarize(aantal_teldagen = n_distinct(doy),
              aantal_waarnemers = n_distinct(waarnemer),
              .groups = "drop") %>%
    filter(aantal_teldagen > 1 | aantal_waarnemers > 1)

  if (nrow(doubles_df) != 0) {
    out_df <- counts_df_state_pro %>%
      semi_join(doubles_df, by = c("plotnaam", "jaar", "periode_in_jaar")) %>%
      # Calculate variables to validate count data in case of doubles
      group_by(plotid, jaar, periode_in_jaar) %>%
      mutate(prof_in_period = any(status_teller == "professioneel"),
             max_doy = (doy == max(doy))
      ) %>%
      arrange(plotid, jaar, periode_in_jaar, doy, waarnemer) %>%
      ungroup() %>%
      rowwise() %>%
      # Keep observations of professionals or otherwise the last day
      mutate(keep = ifelse(status_teller == "professioneel", TRUE,
                           ifelse(!isTRUE(prof_in_period) & isTRUE(max_doy),
                                  TRUE, FALSE))
      ) %>%
      filter(isTRUE(keep)) %>%
      ungroup() %>%
      # Add non-double count data
      bind_rows(counts_df_state_pro %>%
                  anti_join(doubles_df,
                            by = c("plotnaam", "jaar", "periode_in_jaar"))) %>%
      # Remove added columns
      select(-c(prof_in_period, max_doy, keep))
  } else {
    out_df <- counts_df_state_pro
  }

  return(out_df)
}

# Change Dutch subspecies names to common names
adjust_subspecies_names_nl <- function(counts_df) {
  out_df <- counts_df %>%
    mutate(
      naam = case_when(
        tolower(naam) %in% tolower(c("gele kwikstaart (spec)",
                                     "engelse kwikstaart"))
        ~ "Gele Kwikstaart",
        tolower(naam) %in% tolower(c("witte kwikstaart (spec)",
                                     "Rouwkwikstaart"))
        ~ "Witte Kwikstaart",
        tolower(naam) == tolower("canadese gans spec.")
        ~ "Grote Canadese Gans",
        tolower(naam) == tolower("Witsterblauwborst")
        ~ "Blauwborst",
        TRUE ~ naam
      )
    )

  return(out_df)
}
