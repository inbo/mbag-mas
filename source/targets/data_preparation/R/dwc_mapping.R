# Convert Lambert to decimal degrees
spatial_mapping <- function(data_df) {
  require("dplyr")
  require("rlang")
  require("sf")


  out_df <- data_df %>%
    # Remove Amersfoord coordinates
    select(
      -"raw_x_amersfoord",
      -"raw_y_amersfoord"
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
      dwc_type                 = "??",
      dwc_datasetID            = "??",
      dwc_language             = "en",
      dwc_license              = paste0("http://creativecommons.org/",
                                        "publicdomain/zero/1.0/"),
      dwc_publisher            = paste0("Research Institute for Nature and",
                                        "Forest (INBO)"),
      dwc_rightsHolder         = paste0("Research Institute for Nature and",
                                        "Forest (INBO)"),
      dwc_accessRights         = "http://www.inbo.be/en/norms-for-data-use",
      dwc_institutionCode      = "INBO",
      dwc_datasetName          = "??",
      dwc_collectionCode       = "MAS",
      dwc_kingdom              = "Animalia",
      dwc_nomenclaturalCode    = "ICZN",
      dwc_eventType            = "Survey",
      dwc_samplingProtocol     = "??",
      dwc_samplingEffort       = "??",
      dwc_continent            = "Europe",
      dwc_country              = "Belgium",
      dwc_stateProvince        = "Flanders",
      dwc_countryCode          = "BE",
      dwc_basisOfRecord        = "HumanObservation",
      dwc_organismQuantityType = "individuals",
      dwc_coordinateUncertaintyInMeters = "5"
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
      "dwc_taxonID"            = "raw_soortnr"
    ) %>%
    mutate(
      dwc_identifiedBy = .data$dwc_recordedBy,
      dwc_individualCount = .data$dwc_organismQuantity
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
      dwc_occurrenceStatus = ifelse(.data$dwc_individualCount > 0,
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
      )) %>%
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


# Finalise DwC data
finalise_dwc_df <- function(data_df, taxonomy_df) {
  require("dplyr")
  require("rlang")

  # Do manual mapping for difficult taxa
  df_veldmuizen <- rgbif::name_usage("2438591")$data %>%
    select(all_of(
      c("scientificName", "phylum", "order", "family", "genus", "authorship",
        "rank", "key")
      )
    ) %>%
    rename("speciesKey" = "key")

  df_ratten <- rgbif::name_usage("2439223")$data %>%
    select(all_of(
      c("scientificName", "phylum", "order", "family", "genus", "authorship",
        "rank", "key")
    )
    ) %>%
    rename("speciesKey" = "key")

  df_spitsmuizen <- rgbif::name_usage("5534")$data %>%
    select(all_of(
      c("scientificName", "phylum", "order", "family", "authorship",
        "rank", "key")
    )
    ) %>%
    rename("speciesKey" = "key")

  # Create dataframe for merging difficult taxa
  manual_taxa_df <- tibble(
    dwc_vernacularName = c(
      "Veldmuis/Aardmuis",
      "rat spec.",
      "spitsmuis spec."
    )) %>%
    bind_cols(
      bind_rows(
        df_veldmuizen,
        df_ratten,
        df_spitsmuizen
      )
    )

  # Finish manual taxon mapping
  taxon_core_final <- taxonomy_df %>%
    # Add taxon info difficult names
    left_join(manual_taxa_df,
              by = "dwc_vernacularName",
              suffix = c("", ".df2")) %>%
    mutate(
      scientificName = coalesce(scientificName, scientificName.df2),
      phylum = coalesce(phylum, phylum.df2),
      order = coalesce(order, order.df2),
      family = coalesce(family, family.df2),
      genus = coalesce(genus, genus.df2),
      authorship = coalesce(authorship, authorship.df2),
      rank = coalesce(rank, rank.df2),
      speciesKey = coalesce(speciesKey, speciesKey.df2)
    ) %>%
    select(-ends_with(".df2")) %>%
    # Join with observations dataset
    full_join(
      data_df,
      relationship = "many-to-many",
      by = c("dwc_taxonID", "dwc_vernacularName", "dwc_class", "dwc_kingdom")
    ) %>%
    # Select and rename columns
    rename(
      "dwc_scientificNameID"         = "speciesKey",
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
    "basisOfRecord", "eventType", "eventID",
    "occurrenceID", "recordedBy", "individualCount", "organismQuantity",
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
