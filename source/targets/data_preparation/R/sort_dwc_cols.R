sort_dwc_cols <- function(df, col_order = NULL) {
  if (is.null(col_order)) {
    col_order <- c(
      # --- Metadata / Dataset ---
      "datasetID", "datasetName", "type", "language", "license",
      "rightsHolder", "accessRights", "collectionCode", "institutionCode",
      "institutionID",

      # --- Occurrence Core ---
      "occurrenceID", "basisOfRecord",

      # --- Event ---
      "eventID", "parentEventID", "eventType",
      "samplingProtocol", "samplingEffort", "eventDate",
      "year", "month", "day",

      # --- Occurrence ---
      "recordedBy",
      "organismQuantity", "organismQuantityType", "lifeStage",
      "occurrenceStatus", "behavior", "verbatimBehavior", "recordNumber",
      "occurrenceRemarks", "dynamicProperties",

      # --- Location / Georeference ---
      "continent", "country", "countryCode", "stateProvince", "locationID",
      "verbatimLatitude", "verbatimLongitude", "verbatimCoordinateSystem",
      "verbatimSRS", "decimalLatitude", "decimalLongitude",
      "geodeticDatum", "coordinateUncertaintyInMeters",
      "georeferenceRemarks", "dataGeneralizations", "informationWithheld",

      # --- Identification ---
      "identifiedBy", "identificationVerificationStatus",
      "verbatimIdentification", "identificationQualifier",

      # --- Taxonomy ---
      "taxonID", "scientificName", "scientificNameAuthorship",
      "scientificNameID", "taxonRank", "verbatimTaxonRank", "nomenclaturalCode",
      "vernacularName",
      "kingdom", "phylum", "class", "order", "family", "genus",
      "specificEpithet"
    )
  }

  stopifnot("All column names of `df` should be present in `col_order`." =
              length(setdiff(names(df), col_order)) == 0)

  return(df[, intersect(col_order, names(df))])
}
