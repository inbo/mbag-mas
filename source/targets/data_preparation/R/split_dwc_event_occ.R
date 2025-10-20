split_dwc_event_occ <- function(df) {
  # Event file: metadata + event + location
  event_fields <- c(
    "type", "language", "license", "publisher", "rightsHolder", "accessRights",
    "collectionCode", "institutionCode",
    "eventID", "parentEventID", "eventType", "samplingProtocol",
    "samplingEffort", "eventDate", "year", "month", "day",
    "continent", "country", "countryCode", "stateProvince", "locationID",
    "verbatimLatitude", "verbatimLongitude", "verbatimCoordinateSystem",
    "verbatimSRS", "decimalLatitude", "decimalLongitude",
    "geodeticDatum", "coordinateUncertaintyInMeters"
  )

  # Occurrence file: metadata + event + occurrence + identification + taxonomy
  occ_fields <- c(
    "type", "language", "license", "publisher", "rightsHolder", "accessRights",
    "collectionCode", "institutionCode",
    "eventID", "occurrenceID", "basisOfRecord",
    "recordedBy", "organismQuantity", "organismQuantityType", "lifeStage",
    "occurrenceStatus", "behavior", "verbatimBehavior",
    "occurrenceRemarks",
    "identifiedBy", "identificationVerificationStatus", "vernacularName",
    "taxonID", "scientificName", "scientificNameAuthorship",
    "scientificNameID", "taxonRank", "nomenclaturalCode",
    "verbatimIdentification", "identificationQualifier", "verbatimTaxonRank",
    "kingdom", "phylum", "class", "order", "family", "genus", "species"
  )

  # Select existing columns
  events_df <- df[, intersect(event_fields, names(df)), drop = FALSE]
  occ_df    <- df[, intersect(occ_fields,  names(df)), drop = FALSE]

  return(list(events = events_df, occurrences = occ_df))
}
