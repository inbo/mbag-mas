# Transform Amersfoort coordinates to Lambert 72 correctly
# see https://inbo.github.io/tutorials/tutorials/spatial_transform_crs_2/
amersfoort_to_lambert72 <- function(sf_object) {
  require("dplyr")
  require("rlang")
  require("sf")

  sf_proj_network(TRUE)

  # Transform CRS according to specific pipeline
  pipelines <- sf_proj_pipelines("EPSG:28992", "EPSG:31370")

  # We select the pipeline with lowest accuracy, by filtering on accuracy
  chosen_pipeline_definition <- pipelines %>%
    slice_min(.data$accuracy, n = 1) %>%
    pull(.data$definition)

  # Transform according to most accurate pipeline
  out_sf <-
    st_transform(st_geometry(sf_object), "EPSG:31370",
                 pipeline = chosen_pipeline_definition
    ) %>%
    st_sf(st_drop_geometry(sf_object)) %>%
    rename(geometry = ".") %>%
    as_tibble() %>%
    st_as_sf() %>%
    rename(
      x_amersfoord = "x_coord",
      y_amersfoord = "y_coord"
    ) %>%
    mutate(
      x_lambert = unlist(purrr::map(.data$geometry, 1)),
      y_lambert = unlist(purrr::map(.data$geometry, 2))
    ) %>%
    select("oid":"y_amersfoord", "x_lambert", "y_lambert", everything())

  return(out_sf)
}

# Function to remove data counted twice at the same spot
process_double_counted_data <- function(counts_df) {
  require("dplyr")
  require("rlang")

  # Professional bird counters
  profs <- c("WVNT00", "JJNN16", "NOVN00", "ETBX00")
  profs_2022 <- "RPLT02" # professional after 2022

  counts_df_state_pro <- counts_df %>%
    rename(waarnemer = "waarneme") %>%
    mutate(status_teller = ifelse(
      (.data$waarnemer %in% profs) |
        (.data$waarnemer %in% profs_2022 & .data$jaar >= 2022),
      "professioneel", "vrijwilliger"))

  # Identify points counted more than once per period
  doubles_df <- counts_df_state_pro %>%
    st_drop_geometry() %>%
    group_by(.data$plotnaam, .data$jaar, .data$periode_in_jaar) %>%
    summarize(aantal_teldagen = n_distinct(.data$doy),
              aantal_waarnemers = n_distinct(.data$waarnemer),
              .groups = "drop") %>%
    filter(.data$aantal_teldagen > 1 | .data$aantal_waarnemers > 1)

  if (nrow(doubles_df) != 0) {
    out_df <- counts_df_state_pro %>%
      semi_join(doubles_df, by = c("plotnaam", "jaar", "periode_in_jaar")) %>%
      # Calculate variables to validate count data in case of doubles
      group_by(.data$plotid, .data$jaar, .data$periode_in_jaar) %>%
      mutate(prof_in_period = any(.data$status_teller == "professioneel"),
             max_doy = (.data$doy == max(.data$doy))
      ) %>%
      arrange(.data$plotid, .data$jaar, .data$periode_in_jaar, .data$doy,
              .data$waarnemer) %>%
      ungroup() %>%
      rowwise() %>%
      # Keep observations of professionals or otherwise the last day
      mutate(keep = ifelse(
        .data$status_teller == "professioneel",
        TRUE,
        ifelse(!isTRUE(.data$prof_in_period) & isTRUE(.data$max_doy),
               TRUE, FALSE))
      ) %>%
      filter(isTRUE(.data$keep)) %>%
      ungroup() %>%
      # Add non-double count data
      bind_rows(counts_df_state_pro %>%
                  anti_join(doubles_df,
                            by = c("plotnaam", "jaar", "periode_in_jaar"))) %>%
      # Remove added columns
      select(-c("prof_in_period", "max_doy", "keep"))
  } else {
    out_df <- counts_df_state_pro
  }

  return(out_df)
}

# Change Dutch subspecies names to common names
adjust_subspecies_names_nl <- function(counts_df) {
  require("dplyr")

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

# Add predator variables to dataframe
add_predator_variables <- function(counts_df) {
  require("dplyr")
  require("rlang")

  # Load predator species
  predatoren <- predatoren_f()

  out_df <- counts_df %>%
    # Calculate number of predators per observation
    mutate(predator = ifelse(.data$naam %in% predatoren, "ja", "nee"),
           predator_num = ifelse(.data$naam %in% predatoren,
                                 .data$aantal,
                                 0)) %>%
    # Calculate number of predators per location and time
    group_by(.data$plotnaam, .data$jaar, .data$periode_in_jaar) %>%
    mutate(n_predators_plot = sum(.data$predator_num)) %>%
    ungroup() %>%
    select(-c("predator_num"))

  return(out_df)
}
