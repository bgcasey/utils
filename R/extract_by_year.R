#' Extract Raster Values by Year
#'
#' Extracts raster values for each feature in an `sf` object based on
#' the `year` column. Returns a data frame of extracted values with
#' all original columns retained and a single set of band columns.
#'
#' @param sf_object sf, a spatial features object with a `year` column.
#' @param raster_list list, a named list of raster objects, where names
#' contain years in the format `name_name_year`.
#' @param extract_args list, optional arguments passed to the
#' `terra::extract` function (e.g., `exact`, `ID`).
#' @return data.frame, a data frame containing all original columns
#' and band columns with extracted raster values.
#'
#' @examples
#' # Example data
#' sf_object <- sf::st_as_sf(data.frame(
#'     site = c("A", "B"),
#'     year = c(2020, 2021),
#'     longitude = c(-114.07, -113.49),
#'     latitude = c(51.05, 53.55)
#' ), coords = c("longitude", "latitude"), crs = 4326)
#'
#' raster_list <- list(
#'     "data_2020" = terra::rast(array(1:30, dim = c(5, 6, 3)),
#'         extent = c(-120, -110, 50, 55)
#'     ),
#'     "data_2021" = terra::rast(array(31:60, dim = c(5, 6, 3)),
#'         extent = c(-120, -110, 50, 55)
#'     )
#' )
#' names(raster_list[["data_2020"]]) <- c("Band1", "Band2", "Band3")
#' names(raster_list[["data_2021"]]) <- c("Band1", "Band2", "Band3")
#'
#' # Example function call
#' extracted_values <- extract_by_year(
#'     sf_object, raster_list,
#'     extract_args = list(fun = "mean")
#' )
#'
#' # Example result printing
#' print(extracted_values)
#' @export
extract_by_year <- function(sf_object, raster_list, extract_args = list()) {
    # Step 1: Extract years from raster names
    raster_years <- sapply(
        names(raster_list),
        function(name) {
            as.numeric(sub(".*_(\\d+)$", "\\1", name))
        }
    )

    # Step 2: Initialize a result data frame
    result <- as.data.frame(sf_object) # Retain all original columns
    result <- result[, !names(result) %in% "geometry"] # Drop geometry

    # Step 3: Add a single set of columns for band names
    band_names <- names(raster_list[[1]]) # Assumes all rasters have same band names
    for (band_name in band_names) {
        result[[band_name]] <- NA_real_
    }

    # Step 4: Loop through each feature in the sf object
    for (i in seq_len(nrow(sf_object))) {
        feature_year <- sf_object$year[i]

        # Step 5: Match the raster for the feature's year
        raster_index <- which(raster_years == feature_year)

        if (length(raster_index) == 1) {
            # Step 6: Extract values for the feature from the raster
            raster <- raster_list[[raster_index]]

            # Pass additional arguments to terra::extract
            values <- do.call(
                terra::extract,
                c(list(raster, sf_object[i, ]), extract_args)
            )

            # Step 7: Assign raster band values to the result data frame
            result[i, band_names] <- as.numeric(values[1, band_names])
        }
    }

    # Step 8: Return the result data frame
    return(result)
}
