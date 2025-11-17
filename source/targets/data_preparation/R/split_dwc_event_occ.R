split_dwc_event_occ <- function(df) {
  # Darwin Core terms suitable for the events file
  event_fields <- c(
    "eventID", "parentEventID", "eventType",
    "type", "language", "license", "publisher", "rightsHolder", "accessRights",
    "samplingProtocol", "samplingEffort",
    "eventDate", "year", "month", "day",
    "continent", "country", "countryCode", "stateProvince", "locationID",
    "verbatimLatitude", "verbatimLongitude", "verbatimCoordinateSystem",
    "verbatimSRS", "decimalLatitude", "decimalLongitude",
    "geodeticDatum", "coordinateUncertaintyInMeters",
    # For blurred dataset
    "dataGeneralizations", "georeferenceRemarks"
  )

  # All remaining DwC terms go to occurrence file
  # Include eventID again (to interlink event and occurrence)
  occ_fields <- setdiff(names(df), setdiff(event_fields, "eventID"))

  # Select existing columns
  events_df <- df[, intersect(event_fields, names(df)), drop = FALSE]
  occ_df    <- df[, intersect(occ_fields,  names(df)), drop = FALSE]

  return(list(events = events_df, occurrences = occ_df))
}
