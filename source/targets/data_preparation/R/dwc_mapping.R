# to be continued
dwc_mapping <- function(data_sf) {
  require("dplyr")
  require("rlang")
  require("sf")

  # Data cleaning: remove rows with all NA's and unnecessary columns
  data_sf <- data_sf[, colSums(is.na(data_sf)) < nrow(data_sf)]
  data_sf <- data_sf %>%
    select(-"status_teller",
           -"predator",
           -"n_predators_plot",
           -"broedcode")

  # Spatial processing
  ## Lambert to Decimals: keep original X, Y as verbatim coordinates
  taxon_core <- data_sf %>%
    select(-"x_amersfoord", -"y_amersfoord") %>%
    rename(
      "dwc_verbatimLatitude" = "y_lambert",
      "dwc_verbatimLongitude" = "x_lambert") %>%
    mutate(
      dwc_verbatimCoordinateSystem = "BD72 / Belgian Lambert 72",
      dwc_verbatimSRS = "EPSG:31370") %>%
    st_transform(4326) %>%
    mutate(
      dwc_decimalLatitude = round(st_coordinates(.data$geometry)[, 2], 5),
      dwc_decimalLongitude = round(st_coordinates(.data$geometry)[, 1], 5),
      dwc_geodeticDatum = "EPSG:4326") %>%
    st_drop_geometry()

  # Static values
  ## Static values have the same value for all records
  taxon_core <- taxon_core %>%
    mutate(
      dwc_type = "Event",
      # dwc_datasetID = "insert doi",
      dwc_language = "nl",
      dwc_license = "http://creativecommons.org/publicdomain/zero/1.0/",
      dwc_rightsHolder = "INBO",
      dwc_accessRights = "http://www.inbo.be/en/norms-for-data-use",
      dwc_institutionCode = "INBO",
      #dwc_datasetName = "name",
      dwc_ownerInstitutionCode = "INBO",
      dwc_kingdom = "Animalia",
      dwc_nomenclaturalCode = "ICZN",
      dwc_taxonRank = "species",
      dwc_eventType = "Survey",
      dwc_samplingProtocol = "...",
      dwc_continent = "Europe",
      dwc_country = "Belgium",
      dwc_stateProvince = "Flanders",
      dwc_countryCode = "BE",
      dwc_basisOfRecord = "HumanObservation"
    )

  # Unaltered values
  # The content is an exact copy of the corresponding field of the raw data
  taxon_core <- taxon_core %>%
    rename(
      "dwc_vernacularName" = "naam",
      "dwc_eventDate" = "datum",
      "dwc_year" = "jaar",
      "dwc_month" = "maand",
      "dwc_day" = "dag",
      "dwc_recordedBy" = "waarnemer",
      "dwc_individualCount" = "aantal",
      "dwc_locationID" = "plotnaam",
      "dwc_varbatimBehavior" = "wrntype_omschrijving")


  # Altered values
  taxon_core <- taxon_core %>%
    mutate(
      dwc_occurrenceID = paste0("MBAG:MAS:", oid),
      dwc_eventID = paste0("MBAG:MAS:", dwc_eventDate, dwc_locationID),
      dwc_class = ifelse(.data$soortgrp == 2, "Aves", "Mammalia"),
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
    select(-"soortgrp",
           -"wrntype")

  # Presence absence data
  taxon_core <- taxon_core
  # pivot add zeroes dwc_individualCount = ifelse(dwc_individualCount > 0, "Present", "Absent")

  glimpse(taxon_core)
}
