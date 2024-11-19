# Convert Lambert to decimal degrees
spatial_mapping <- function(data_df) {
  require("dplyr")
  require("rlang")
  require("sf")


  out_df <- data_df %>%
    # Remove Amersfoort coordinates
    select(
      -"raw_x_amersfoort",
      -"raw_y_amersfoort"
    ) %>%
    # Keep original X, Y as verbatim coordinates
    rename(
      "dwc_verbatimLatitude" = "raw_y_lambert",
      "dwc_verbatimLongitude" = "raw_x_lambert"
    ) %>%
    # Add CRS
    mutate(
      dwc_verbatimCoordinateSystem = "BD72 / Belgian Lambert 72",
      dwc_verbatimSRS = "EPSG:31370"
    ) %>%
    # Convert to WGS 84 decimal coordinates
    st_transform(4326) %>%
    mutate(
      dwc_decimalLatitude = round(st_coordinates(.data$raw_geometry)[, 2], 5),
      dwc_decimalLongitude = round(st_coordinates(.data$raw_geometry)[, 1], 5),
      dwc_geodeticDatum = "EPSG:4326"
    ) %>%
    st_drop_geometry()

  return(out_df)
}

# Static values are used for Darwin Core terms that need the same value for all
# records. They are usually missing from the input data.
static_mapping <- function(data_df) {
  require("dplyr")

  out_df <- data_df %>%
    mutate(
      dwc_type                 = NA,
      dwc_datasetID            = NA,
      dwc_language             = "en",
      dwc_license              = paste0("http://creativecommons.org/",
                                        "publicdomain/zero/1.0/"),
      dwc_publisher            = paste0("Research Institute for Nature and",
                                        "Forest (INBO)"),
      dwc_rightsHolder         = paste0("Research Institute for Nature and",
                                        "Forest (INBO)"),
      dwc_accessRights         = "http://www.inbo.be/en/norms-for-data-use",
      dwc_institutionCode      = "INBO",
      dwc_datasetName          = NA,
      dwc_collectionCode       = "MAS",
      dwc_kingdom              = "Animalia",
      dwc_nomenclaturalCode    = "ICZN",
      dwc_eventType            = "Survey",
      dwc_samplingProtocol     = NA,
      dwc_samplingEffort       = NA,
      dwc_continent            = "Europe",
      dwc_country              = "Belgium",
      dwc_stateProvince        = "Flanders",
      dwc_countryCode          = "BE",
      dwc_basisOfRecord        = "HumanObservation"
    )

  return(out_df)
}

# Unchanged values are used for Darwin Core terms whose content is an exact
# copy of the corresponding field in the input data.
unchanged_mapping <- function(data_df) {
  require("dplyr")
  require("rlang")

  out_df <- data_df %>%
    rename(
      "dwc_vernacularName"     = "raw_naam",
      "dwc_eventDate"          = "raw_datum",
      "dwc_year"               = "raw_jaar",
      "dwc_month"              = "raw_maand",
      "dwc_day"                = "raw_dag",
      "dwc_recordedBy"         = "raw_waarnemer",
      "dwc_organismQuantity"   = "raw_aantal",
      "dwc_locationID"         = "raw_plotnaam",
      "dwc_varbatimBehavior"   = "raw_wrntype_omschrijving",
      "dwc_occurrenceRemarks"  = "raw_opmerk",
      "dwc_taxonID"            = "raw_soortnr",
      "dwc_is_mas_sample"         = "raw_is_mas_sample"
    ) %>%
    mutate(
      dwc_identifiedBy = .data$dwc_recordedBy
    )

  return(out_df)
}

# Modified values are used for Darwin Core terms where the content in the input
# data are used as a basis, but this should be standardized. This applies to
# Darwin Core terms where we use a vocabulary or where we want to transform for
# clarity or to correct obvious errors.
modified_mapping <- function(data_df) {
  require("dplyr")

  out_df <- data_df %>%
    mutate(
      dwc_occurrenceID = paste0("MBAG:MAS:", .data$raw_oid),
      dwc_eventID = paste0("MBAG:MAS:",
                           .data$dwc_eventDate,
                           .data$dwc_locationID),
      dwc_class = ifelse(.data$raw_soortgrp == 2, "Aves", "Mammalia"),
      dwc_occurrenceStatus = ifelse(.data$dwc_organismQuantity > 0,
                                    "Present", "Absent"),
      dwc_behavior = case_when(
        .data$dwc_varbatimBehavior == "Territoriaal gedrag" ~
          "Teritorial behaviour",
        .data$dwc_varbatimBehavior == "Individu of groep niet plaatsgebonden" ~
          "Individual or group not bound to a location",
        .data$dwc_varbatimBehavior == "Volwassen individu in broedbiotoop" ~
          "Adult individual in breeding habitat",
        .data$dwc_varbatimBehavior == "Nestvondst" ~
          "Nest discovery",
        .data$dwc_varbatimBehavior == "Nest-aanduidend gedrag" ~
          "Nest-indicating behaviour",
        .data$dwc_varbatimBehavior == "Paar in broedbiotoop" ~
          "Pair in breeding habitat"
        ),
      # If the distance is < 100 m --> 10 m
      # If the distance is >= 100 m --> 0.1 * distance
      # If the distance is unknown --> 30 m (0.1 * 300 m)
      dwc_coordinateUncertaintyInMeters =
        ifelse(is.na(.data$raw_distance2plot), 30,
          ifelse(.data$raw_distance2plot < 100,
                 10, 0.1 * .data$raw_distance2plot)),
      dwc_organismQuantityType = ifelse(.data$raw_wrntype == "0",
                                        "individuals", "breeding pairs")
    ) %>%
    select(
      -"raw_oid",
      -"raw_soortgrp",
      -"raw_wrntype"
    )

  return(out_df)
}

# Darwin Core mapping function
dwc_mapping <- function(data_df) {
  require("dplyr")
  require("sf")

  # Preparation
  ## Remove columns related to sampling frame
  raw_data <- data_df %>%
    select(
      -"batch",
      -"sample_order"
    )

  ## Add prefix `raw_` to the column names of `raw_data`
  colnames(raw_data) <- paste0("raw_", colnames(raw_data))
  st_geometry(raw_data) <- "raw_geometry"


  # Spatial processing
  spatial_df <- spatial_mapping(raw_data)

  # Static values DwC mapping
  static_df <- static_mapping(spatial_df)

  # Unchanged values DwC mapping
  unchanged_df <- unchanged_mapping(static_df)

  # Modified values DwC mapping
  out_df <- modified_mapping(unchanged_df)

  return(out_df)
}

map_taxa_manual <- function(
    taxonomy_df,
    manual_taxon_list,
    vernacular_name_col = "vernacularName",
    out_cols = "scientificName") {
  require("dplyr")
  require("rlang")

  # Get taxonomic info for provided taxon keys
  mapped_taxa_list <- lapply(manual_taxon_list, function(key) {
      taxon_data <- rgbif::name_usage(key)$data
      cols_to_get <- intersect(colnames(taxon_data), out_cols)

      taxon_data %>%
        select(all_of(cols_to_get))
    })
  mapped_taxa_df <- do.call(bind_rows, mapped_taxa_list) %>%
    mutate(!!vernacular_name_col := names(manual_taxon_list))


  # Add taxon info difficult names
  out_df <- taxonomy_df %>%
    left_join(
      mapped_taxa_df,
      by = "dwc_vernacularName",
      suffix = c("", ".df2")) %>%
    mutate(
      across(
        all_of(setdiff(colnames(mapped_taxa_df), vernacular_name_col)),
        ~ coalesce(.x, get(paste0(cur_column(), ".df2"))),
        .names = "{.col}")
    ) %>%
    select(-ends_with(".df2"))

  return(out_df)
}

# Finalise DwC data
finalise_dwc_df <- function(data_df, taxonomy_df) {
  require("dplyr")
  require("rlang")

  # Join with observations dataset
  taxon_core_final <- taxonomy_df %>%
    full_join(
      data_df,
      relationship = "many-to-many",
      by = c("dwc_taxonID", "dwc_vernacularName", "dwc_class", "dwc_kingdom")
    ) %>%
    # Select and rename columns
    rename(
      "dwc_scientificNameID"         = "key",
      "dwc_taxonRank"                = "rank",
      "dwc_scientificNameAuthorship" = "authorship"
    ) %>%
    select(-"tar_group")

  # Remove raw columns
  out_df <- taxon_core_final %>%
    select(-starts_with("raw_")) %>%
    rename_with(~ gsub("dwc\\_", "", .x))

  # Select and sort columns
  col_order <- c(
    "type", "language", "license", "publisher", "rightsHolder", "accessRights",
    "datasetID", "collectionCode", "institutionCode", "datasetName",
    "basisOfRecord", "eventType", "eventID", "is_mas_sample",
    "occurrenceID", "recordedBy", "organismQuantity",
    "organismQuantityType", "occurrenceStatus", "behavior", "varbatimBehavior",
    "occurrenceRemarks", "samplingProtocol", "samplingEffort", "eventDate",
    "day", "month", "year", "continent", "country", "countryCode",
    "stateProvince", "locationID", "verbatimLatitude", "verbatimLongitude",
    "verbatimCoordinateSystem", "verbatimSRS", "decimalLatitude",
    "decimalLongitude", "geodeticDatum", "coordinateUncertaintyInMeters",
    "identifiedBy", "vernacularName", "taxonID", "scientificName", "kingdom",
    "phylum", "class", "order", "family", "genus", "species",
    "scientificNameAuthorship", "scientificNameID", "taxonRank",
    "nomenclaturalCode"
  )
  out_df <- out_df[, col_order]

  return(out_df)
}
