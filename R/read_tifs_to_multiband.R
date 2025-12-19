#' Read TIF Files into a Multiband Raster Object
#'
#' Reads all `.tif` files from a specified directory and combines
#' them into a single multiband raster object. Each band in the
#' resulting raster corresponds to one of the TIF files, and the
#' band names are derived from the TIF file names (without the
#' `.tif` extension).
#'
#' @param tif_directory Character, the path to the directory
#' containing `.tif` files.
#' @return A multiband raster object (`SpatRaster`) where each band
#' corresponds to a `.tif` file in the directory, named after the
#' corresponding file.
#'
#' @examples
#' \dontrun{
#' # Example usage of the function
#' tif_directory <- "path/to/tif/files"
#' multiband_raster <- read_tifs_to_multiband(tif_directory)
#' print(multiband_raster) # Display the multiband raster object
#' plot(multiband_raster) # Plot the raster bands
#' }
#' @export
read_tifs_to_multiband <- function(tif_directory) {
    # Step 1: List all TIF files in the specified directory
    tif_files <- list.files(
        path       = tif_directory,
        pattern    = "\\.tif$",
        full.names = TRUE
    )

    # Step 2: Check if any TIF files were found
    if (length(tif_files) == 0) {
        stop("No .tif files found in the specified directory.")
    }

    # Step 3: Read all TIF files into a list of raster objects
    raster_list <- lapply(tif_files, terra::rast)

    # Step 4: Combine the raster objects into a single multiband raster
    multiband_raster <- do.call(c, raster_list)

    # Step 5: Assign band names based on the file names
    band_names <- gsub("\\.tif$", "", basename(tif_files))
    names(multiband_raster) <- band_names

    # Step 6: Return the multiband raster object
    return(multiband_raster)
}
