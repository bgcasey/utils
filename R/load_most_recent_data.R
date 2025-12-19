#' Load the Most Recent Data File Dynamically
#'
#' This function dynamically loads the most recent `.rData` file
#' from a specified directory that matches a given file prefix and
#' follows the naming convention: [prefix]YYYY-MM-DD.rData.
#'
#' @param directory character. Path to the directory containing
#'   the `.rData` files.
#' @param file_prefix character. Prefix of the file to search for
#'   (e.g., "data_for_models_").
#'
#' @return Loads the data from the most recent `.rData` file into
#'   the global environment and prints the name of the loaded file.
#'
#' @example
#' # Example usage
#' load_most_recent_data(
#'   directory = "2_pipeline/",
#'   file_prefix = "data_for_models_"
#' )
#'
load_most_recent_data <- function(directory, file_prefix) {
    # Step 1: List all files matching the prefix and pattern
    files <- list.files(
        path = directory,
        pattern = paste0("^", file_prefix, "\\d{4}-\\d{2}-\\d{2}\\.RData$"),
        full.names = TRUE
    )

    # Step 2: Check if any matching files exist
    if (length(files) == 0) {
        stop("No matching files found in the directory.")
    }

    # Step 3: Extract dates from filenames
    dates <- gsub(
        paste0("^", file_prefix, "(\\d{4}-\\d{2}-\\d{2})\\.RData$"),
        "\\1",
        basename(files)
    )

    # Step 4: Convert extracted dates to Date objects
    dates <- suppressWarnings(as.Date(dates, format = "%Y-%m-%d"))

    # Step 5: Validate if dates were properly parsed
    if (any(is.na(dates))) {
        stop(
            "Failed to parse dates from filenames. Ensure filenames match ",
            "the format [prefix]YYYY-MM-DD.rData."
        )
    }

    # Step 6: Find the most recent file
    most_recent_file <- files[which.max(dates)]

    # Step 7: Load the most recent file into the global environment
    load(most_recent_file, envir = .GlobalEnv)
    message("Loaded file: ", most_recent_file)
}
