split_dwc_event_occ <- function(df) {
  # Remove unnecessary columns
  df <- dplyr::select(df, -"verbatimBehavior")

  # Terms for both datasets
  double_terms <- c(
    "eventID", "dynamicProperties", "license", "publisher", "rightsHolder",
    "accessRights", "institutionID", "datasetName"
  )

  # Darwin Core terms suitable for the events file
  event_fields <- c(
    double_terms,
    "parentEventID", "eventType", "type", "language",
    "samplingProtocol", "samplingEffort",
    "eventDate", "year", "month", "day",
    "continent", "country", "countryCode", "stateProvince", "locationID",
    "verbatimLatitude", "verbatimLongitude", "verbatimCoordinateSystem",
    "verbatimSRS", "decimalLatitude", "decimalLongitude",
    "geodeticDatum", "coordinateUncertaintyInMeters",
    # For blurred dataset
    "dataGeneralizations", "georeferenceRemarks", "informationWithheld"
  )

  # All remaining DwC terms go to occurrence file
  # Include double DwC terms again
  occ_fields <- setdiff(
    names(df),
    setdiff(event_fields, double_terms)
  )

  # Select existing columns
  events_df <- dplyr::distinct(
    df[, intersect(event_fields, names(df)), drop = FALSE]
  )
  occ_df <- df[, intersect(occ_fields,  names(df)), drop = FALSE]

  # Return list of datasets (sort columns)
  return(
    list(
      events = sort_dwc_cols(events_df),  # nolint: object_usage_linter
      occurrences = sort_dwc_cols(occ_df) # nolint: object_usage_linter
    )
  )
}
