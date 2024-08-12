# Convert Lambert to decimal degrees
spatial_mapping <- function(data_sf) {
  require("dplyr")
  require("rlang")
  require("sf")


  out_df <- data_sf %>%
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
static_mapping <- function(data_sf) {
  require("dplyr")

  out_df <- data_sf %>%
    mutate(
      dwc_type                 = "??",
      dwc_datasetID            = "??",
      dwc_language             = "en",
      dwc_license              = paste0("http://creativecommons.org/",
                                        "publicdomain/zero/1.0/"),
      dwc_publisher            = paste0("Research Institute for Nature and",
                                        "Forest (INBO)"),
      dwc_rightsHolder         = "INBO",
      dwc_accessRights         = "http://www.inbo.be/en/norms-for-data-use",
      dwc_institutionCode      = "INBO",
      dwc_datasetName          = "??",
      dwc_ownerInstitutionCode = "INBO",
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
      dwc_basisOfRecord        = "HumanObservation"
    )

  return(out_df)
}

# Unchanged values are used for Darwin Core terms whose content is an exact
# copy of the corresponding field in the input data.
unchanged_mapping <- function(data_sf) {
  require("dplyr")

  out_df <- data_sf %>%
    rename(
      "dwc_vernacularName"    = "raw_naam",
      "dwc_eventDate"         = "raw_datum",
      "dwc_year"              = "raw_jaar",
      "dwc_month"             = "raw_maand",
      "dwc_day"               = "raw_dag",
      "dwc_recordedBy"        = "raw_waarnemer",
      "dwc_individualCount"   = "raw_aantal",
      "dwc_locationID"        = "raw_plotnaam",
      "dwc_varbatimBehavior"  = "raw_wrntype_omschrijving",
      "dwc_occurrenceRemarks" = "raw_opmerk",
      "dwc_taxonID"           = "raw_soortnr"
    )

  return(out_df)
}

# Modified values are used for Darwin Core terms where the content in the input
# data are used as a basis, but this should be standardized. This applies to
# Darwin Core terms where we use a vocabulary or where we want to transform for
# clarity or to correct obvious errors.
modified_mapping <- function(data_sf) {
  require("dplyr")

  out_df <- data_sf %>%
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
dwc_mapping <- function(data_sf) {
  require("dplyr")
  require("sf")

  # Preparation
  ## Remove columns related to sampling frame
  raw_data <- data_sf %>%
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
