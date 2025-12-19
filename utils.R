#' Add Alberta flag to a site data frame using precise boundaries
#'
#' Uses rnaturalearth and sf to determine if each site is in Alberta.
#'
#' @param dat Data frame with latitude and longitude columns
#' @param lat_col Name of latitude column (default: "latitude")
#' @param lon_col Name of longitude column (default: "longitude")
#' @return Data frame with in_alberta column ("in" or "out")
#' @import dplyr, sf, rnaturalearth
#' @export
add_alberta_flag <- function(
    dat,
    lat_col = "latitude",
    lon_col = "longitude") {
  # Load required packages
  requireNamespace("sf")
  requireNamespace("dplyr")
  requireNamespace("rnaturalearth")
  requireNamespace("rnaturalearthhires", quietly = TRUE)

  # Assign a temporary unique row identifier
  dat$.row_id_tmp <- seq_len(nrow(dat))

  # Get Canada provinces polygons
  can_prov <- rnaturalearth::ne_states(
    country = "Canada",
    returnclass = "sf"
  )

  # Filter for Alberta
  ab <- can_prov %>%
    dplyr::filter(name_en == "Alberta")

  # Convert input data to sf
  dat_sf <- dat %>%
    dplyr::filter(
      !is.na(.data[[lat_col]]),
      !is.na(.data[[lon_col]])
    ) %>%
    sf::st_as_sf(
      coords = c(lon_col, lat_col),
      crs = sf::st_crs(ab)
    )

  # Spatial join: flag if site is in Alberta
  dat_sf$in_alberta <- sf::st_within(
    dat_sf, ab, sparse = FALSE
  )[, 1]

  # Join back to original data using the unique row id
  dat <- dat %>%
    dplyr::left_join(
      dat_sf %>%
        dplyr::select(.row_id_tmp, in_alberta) %>%
        sf::st_drop_geometry(),
      by = ".row_id_tmp"
    )

  # Remove the temporary identifier
  dat$.row_id_tmp <- NULL

  return(dat)
}

#' Estimate Total Processing Time
#'
#' Calculates an estimated total processing time for a complete
#' dataset based on the time taken to process a subset of its
#' features. The result is returned as a human-readable string
#' indicating days, hours, minutes, and remaining seconds.
#'
#' @param n_features Numeric. The total number of features in the
#'   dataset.
#' @param nsub Numeric. The number of features actually processed in
#'   the partial subset.
#' @param end_time POSIXct. The time the subset processing ended.
#' @param start_time POSIXct. The time the subset processing started.
#'
#' @return Character string. A human-readable duration representing
#'   the estimated total processing time for all features.
#'
#' @examples
#' # Example usage of the function
#'
#' # Simulate a short processing run
#' start_time <- Sys.time()
#' Sys.sleep(2) # stand-in for some real processing
#' end_time <- Sys.time()
#'
#' # Estimate total time if the above was a test on 100 out of
#' # 10,000 features
#' estimate_processing_time(10000, 100, end_time, start_time)
#'
estimate_processing_time <- function(n_features,
                                     nsub,
                                     end_time,
                                     start_time) {
  # Step 1: Define a helper function to convert seconds to a
  #         human-readable string (days, hours, minutes, seconds).
  convert_seconds <- function(total_seconds) {
    # Round to nearest whole second
    total_seconds <- round(total_seconds)

    sec_in_day <- 86400 # 24 * 3600
    sec_in_hour <- 3600 # 60 * 60
    sec_in_minute <- 60

    days <- total_seconds %/% sec_in_day
    remainder <- total_seconds %% sec_in_day

    hours <- remainder %/% sec_in_hour
    remainder <- remainder %% sec_in_hour

    minutes <- remainder %/% sec_in_minute
    seconds <- remainder %% sec_in_minute

    paste0(
      days, " days, ", hours, " hours, ", minutes,
      " minutes, ", seconds, " seconds"
    )
  }

  # Step 2: Convert elapsed time (end_time - start_time) into
  #         numeric seconds.
  elapsed_secs <- as.numeric(end_time - start_time, units = "secs")

  # Step 3: Scale the partial-processing time to the full dataset:
  #         total time = (n_features / nsub) * elapsed_secs
  total_seconds <- (n_features / nsub) * elapsed_secs

  # Step 4: Convert the total seconds to a human-readable format.
  duration_readable <- convert_seconds(total_seconds)

  # Step 5: Return the final string.
  return(duration_readable)
}




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
#' sf_object <- st_as_sf(data.frame(
#'   site = c("A", "B"),
#'   year = c(2020, 2021),
#'   longitude = c(-114.07, -113.49),
#'   latitude = c(51.05, 53.55)
#' ), coords = c("longitude", "latitude"), crs = 4326)
#'
#' raster_list <- list(
#'   "data_2020" = terra::rast(array(1:30, dim = c(5, 6, 3)),
#'     extent = c(-120, -110, 50, 55)
#'   ),
#'   "data_2021" = terra::rast(array(31:60, dim = c(5, 6, 3)),
#'     extent = c(-120, -110, 50, 55)
#'   )
#' )
#' names(raster_list[["data_2020"]]) <- c("Band1", "Band2", "Band3")
#' names(raster_list[["data_2021"]]) <- c("Band1", "Band2", "Band3")
#'
#' # Example function call
#' extracted_values <- extract_by_year(
#'   sf_object, raster_list,
#'   extract_args = list(fun = "mean")
#' )
#'
#' # Example result printing
#' print(extracted_values)
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


#' Format Time Difference
#'
#' This function calculates the difference between two timestamps
#' and formats it as days, hours, minutes, and seconds for
#' readability.
#'
#' @param start_time POSIXct. The starting timestamp.
#' @param end_time POSIXct. The ending timestamp.
#' @return Character. A formatted string showing the time
#' difference in days, hours, minutes, and seconds.
#'
#' @example
#' # Example usage of the function
#' start_time <- Sys.time()
#' Sys.sleep(5)  # Simulate a delay
#' end_time <- Sys.time()
#' format_time_diff(start_time, end_time)
format_time_diff <- function(start_time, end_time) {
  # Calculate time difference in seconds
  diff <- as.numeric(difftime(end_time, start_time,
    units = "secs"
  ))

  # Extract days, hours, minutes, and seconds
  days <- floor(diff / 86400) # 1 day = 86400 seconds
  hours <- floor((diff %% 86400) / 3600)
  minutes <- floor((diff %% 3600) / 60)
  seconds <- round(diff %% 60, 2)

  # Format the time difference as a string
  sprintf(
    "%d days, %d hours, %d minutes, %.2f seconds",
    days, hours, minutes, seconds
  )
}

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


#' Load .RData Files into a List
#'
#' This function scans a directory for `.RData` files, loads each file,
#' and stores all objects from the `.RData` files into a single list.
#'
#' @param data_dir Character. The path to the directory containing
#' `.RData` files.
#' @return A list where each element is an object loaded from the
#' `.RData` files.
#'
#' @example
#' # Example usage of the function
#' data_dir <- "path/to/your/data/directory"
#' all_data <- load_rdata_files(data_dir)
#' print(names(all_data)) # Prints the names of loaded objects
#'
load_rdata_files <- function(data_dir) {
  # Step 1: Get a list of all .RData files in the specified directory
  rdata_files <- list.files(
    data_dir,
    pattern = "\\.RData$",
    full.names = TRUE
  )

  # Step 2: Initialize an empty list to store objects from .RData files
  all_data <- list()

  # Step 3: Loop through each .RData file
  for (file in rdata_files) {
    # Step 3.1: Load objects directly into a named list element
    loaded_objects <- load(file)
    for (obj_name in loaded_objects) {
      all_data[[obj_name]] <- get(obj_name)
    }
  }

  # Step 4: Return the list containing all loaded objects
  return(all_data)
}


#' Parallel Extraction of Raster Data Stored in a Directory
#'
#' Executes a parallelized approach to extract values from multi-band
#' raster files using a custom extraction function for each spatial
#' feature in `locations`.
#'
#' @param locations sf or Spatial object. Geographic features for which
#'   raster values are extracted.
#' @param tif_directory Character. The directory path containing the
#'   TIF files.
#' @param bands Character vector. The subset of raster bands to extract
#'   from each TIF file.
#' @param extract_fun Function. The function used to perform the
#'   extraction. Defaults to `extract_by_year` (replace as needed).
#' @param extract_args List. Additional arguments passed on to
#'   `extract_fun`. Defaults to `list(fun = "mean")`.
#' @param extra_exports Character vector. Names of additional objects
#'   or functions to export to the cluster environment, if needed.
#'
#' @return A list of extraction results, one element per feature in
#'   `locations`.
#'
#' @examples
#' # Example usage of the function
#' # Suppose you have a set of locations (e.g., sf polygons or points)
#' # buffers <- sf::read_sf("path/to/shapefile.shp")
#'
#' # result <- parallel_extract_directory(
#' #   locations     = list of location buffers,
#' #   tif_directory = "path/to/tifs",
#' #   bands         = c("SR_B1", "NDVI"),
#' #   extract_fun   = extract_by_year,
#' #   extract_args  = list(fun = "mean")
#' # )
#'
#' # print(result)
#'
parallel_extract_directory <- function(locations,
                                       tif_directory,
                                       bands,
                                       extract_fun = extract_by_year,
                                       extract_args = list(fun = "mean"),
                                       extra_exports = NULL) {
  # Step 1: Determine the number of cores
  num_cores <- min(length(locations), detectCores() - 4)

  # Step 2: Initialize the cluster
  cl <- makeCluster(num_cores)

  # Step 3: Export necessary objects to the cluster
  to_export <- c(
    "locations", "read_tifs_to_list", "extract_fun",
    "extract_args", "extra_exports"
  )
  clusterExport(cl, varlist = to_export, envir = environment())

  clusterEvalQ(cl, {
    library(terra)
    library(sf)
  })

  # Step 4: Parallel processing
  result <- parLapply(cl, locations, function(buffer) {
    library(terra)
    library(sf)

    # 4a. Read rasters
    rasters_list <- read_tifs_to_list(tif_directory)

    # 4b. Keep only specified bands
    rasters_list <- lapply(rasters_list, function(rast_obj) {
      rast_obj[[names(rast_obj) %in% bands]]
    })

    # 4c. Perform extraction
    extract_fun(buffer, rasters_list, extract_args)
  })

  # Step 5: Stop the cluster
  stopCluster(cl)

  # Step 6: Return results
  return(result)
}


#' Read TIF Files into a Named List
#'
#' Reads all `.tif` files from a specified directory and stores
#' them as raster objects in a named list. The names of the list
#' elements are derived from the TIF file names (without the `.tif`
#' extension).
#'
#' @param tif_directory Character, the path to the directory
#' containing `.tif` files.
#' @param tif_list List, an optional existing list to append the
#' raster objects to. Defaults to an empty list.
#' @return A list where each element is a raster object, named
#' after the corresponding TIF file.
#'
#' @examples
#' # Example usage of the function
#' tif_directory <- "path/to/tif/files"
#' tif_list <- read_tifs_to_list(tif_directory)
#' print(names(tif_list)) # Display names of list elements
read_tifs_to_list <- function(tif_directory, tif_list = list()) {
  # Step 1: List all TIF files in the specified directory
  tif_files <- list.files(
    path       = tif_directory,
    pattern    = "\\.tif$",
    full.names = TRUE
  )

  # Step 2: Loop through each TIF file
  for (tif_file in tif_files) {
    # Step 3: Extract the file name without the `.tif` extension
    tif_name <- gsub("\\.tif$", "", basename(tif_file))

    # Step 4: Read the TIF file as a raster object
    raster_obj <- terra::rast(tif_file)

    # Step 5: Add the raster object to the list using the file name
    tif_list[[tif_name]] <- raster_obj
  }

  # Step 6: Return the populated list of raster objects
  return(tif_list)
}

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
#' # Example usage of the function
#' tif_directory <- "path/to/tif/files"
#' multiband_raster <- read_tifs_to_multiband(tif_directory)
#' print(multiband_raster) # Display the multiband raster object
#' plot(multiband_raster) # Plot the raster bands
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


#' Select a single response column
#'
#' This function subsets a dataframe so it keeps only one chosen response
#' variable plus all other columns that are *not* in the set of response
#' variables.
#'
#' @param gbm_data data.frame. The full dataset containing responses
#'   and predictors.
#' @param chosen_response character. The name of the response variable
#'   you want to keep.
#' @param response_vars character vector. A list of all possible
#'   response variables.
#' @param reorder logical. If TRUE, the chosen response column is moved
#'   to the front of the new data frame. Defaults to TRUE.
#'
#' @return A data frame that includes only the chosen response column
#'   (plus all columns that were not in `response_vars`).
#'
#' @examples
#' # Suppose 'response_vars' contains c("sonchus_arvensis", "cirsium_arvense", ...)
#' # and 'gbm_data' is your data frame with those columns plus predictors.
#'
#' # Keep only "cirsium_arvense" as the response:
#' new_data <- select_one_response(
#'   gbm_data        = gbm_data,
#'   chosen_response = "cirsium_arvense",
#'   response_vars   = response_vars,
#'   reorder         = TRUE
#' )
#'
#' # Check the first few columns:
#' head(new_data)
#'
select_one_response <- function(gbm_data,
                                chosen_response,
                                response_vars,
                                reorder = TRUE) {
  # Step 1: Validate that chosen_response is in response_vars
  if (!chosen_response %in% response_vars) {
    stop("The chosen response variable is not in 'response_vars'.")
  }

  # Step 2: Identify non-response columns
  non_response_cols <- setdiff(names(gbm_data), response_vars)

  # Step 3: Build the column vector to keep chosen response plus
  #         all non-response columns
  keep_cols <- c(chosen_response, non_response_cols)

  # Step 4: Subset the data
  new_data <- gbm_data[, keep_cols, drop = FALSE]

  # Step 5: If not reordering, restore the original column order
  #         except for other response variables, which are removed
  if (!reorder) {
    keep_original <- setdiff(
      names(gbm_data),
      setdiff(response_vars, chosen_response)
    )
    new_data <- gbm_data[, keep_original, drop = FALSE]
  }

  return(new_data)
}



#' Convert Strings to Snake Case
#'
#' Converts a vector of strings to snake_case by inserting underscores
#' between words and converting all characters to lowercase. It also
#' replaces spaces with underscores.
#'
#' @param x Character vector. The input strings to be converted.
#' @return A character vector with the converted strings in snake_case.
#'
#' @examples
#' snake_case(c("Normal 1991_2020 AHM", "Normal 1991_2020 bFFP"))
#' # Returns: c("normal_1991_2020_ahm", "normal_1991_2020_b_ffp")
snake_case <- function(x) {
  x <- gsub("[^A-Za-z0-9_ ]", "", x) # Remove special characters
  x <- gsub(" ", "_", x) # Replace spaces with underscores
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x) # Add _ between letters
  # x <- gsub("([0-9])([A-Za-z])", "\\1_\\2", x)  # Add _ between numbers
  # x <- gsub("([A-Za-z])([0-9])", "\\1_\\2", x)  # Add _ between letters and numbers
  x <- gsub("__+", "_", x) # Replace multiple underscores with a single _
  tolower(x) # Convert to lowercase
}

#' Select a Specific Species Column
#'
#' This function selects a specific species column from a dataset while
#' retaining key site and location information.
#'
#' @param data A data frame containing species data in wide format.
#' @param species_name A character string representing the scientific
#'        name of the species.
#' @return A data frame with the selected species column and key metadata
#'         columns (`site`, `year`, `obs_date`, `latitude`, `longitude`).
#'
#' @example
#' # Example dataset
#' invsp_wide <- data.frame(
#'   site = c("1001", "1002"),
#'   year = c(2020, 2021),
#'   obs_date = as.Date(c("2020-06-15", "2021-07-20")),
#'   latitude = c(54.12, 54.45),
#'   longitude = c(-113.5, -114.2),
#'   ranunculus_acris = c(1, 0)
#' )
#'
#' # Selecting a species
#' selected_species_df <- select_species(invsp_wide, "Ranunculus acris")
#'
#' # View result
#' print(selected_species_df)
select_species <- function(data, species_name) {
  # [Step 1: Convert species name to snake_case to ensure consistency]
  species_name <- snake_case(species_name)

  # [Step 2: Define key columns that should always be retained]
  key_cols <- c("site", "year", "obs_date", "latitude", "longitude")

  # [Step 3: Check if the species exists in the dataset, stop if not]
  if (!(species_name %in% names(data))) {
    stop("Species not found in the dataset. Check the spelling and format.")
  }

  # [Step 4: Select only the key columns and the specified species column]
  result <- data %>%
    dplyr::select(all_of(c(key_cols, species_name)))

  # [Step 5: Return the filtered dataset]
  return(result)
}


#' Set Terra Options with Buffer
#'
#' Configure `terra` options to control memory
#' usage, core allocation, and temporary file
#' storage. This function calculates resource
#' allocations based on total system memory,
#' number of cores, resource percentage, and
#' optional memory buffer for spikes. It sets
#' `memfrac`, `memmax`, and `tempdir` options
#' in `terra`.
#'
#' @param total_memory_gb Numeric, total system
#' memory in GB.
#' @param total_cores Integer, total number of
#' cores available.
#' @param resource_percent Numeric, percentage
#' of system resources to allocate for the
#' process.
#' @param priority Character, resource
#' allocation strategy: "memory", "speed", or
#' "default". Default is "default".
#' @param buffer_percent Numeric, additional
#' percentage of memory reserved as a buffer to
#' handle spikes. Default is 10.
#' @param tempdir Character, directory for
#' storing temporary files. Default is
#' `tempdir()`.
#' @return Integer, the number of cores to be
#' used for parallel processing.
#'
#' @examples
#' cores <- set_terra_options_with_buffer(
#'   total_memory_gb = 128,
#'   total_cores = 20,
#'   resource_percent = 50,
#'   priority = "default",
#'   buffer_percent = 10,
#'   tempdir = "D:/r_temp/RtmpWSPfnw"
#' )
#' print(cores)
#'
#' @export
set_terra_options <- function(
    total_memory_gb,
    total_cores,
    resource_percent,
    priority = "default",
    buffer_percent = 10,
    tempdir = tempdir()) {
  # Step 1: Calculate memory and cores allocation
  memory_to_use_gb <- total_memory_gb *
    (resource_percent / 100)
  cores_to_use <- total_cores *
    (resource_percent / 100)

  # Step 2: Adjust cores based on priority
  if (priority == "memory") {
    cores_to_use <- max(
      1,
      floor(cores_to_use / 2)
    )
  } else if (priority == "speed") {
    cores_to_use <- min(
      total_cores,
      ceiling(cores_to_use * 2)
    )
  } else if (priority == "default") {
    cores_to_use <- total_cores
  }

  # Step 3: Calculate memfrac and max memory
  # per core with buffer
  memfrac <- (resource_percent / 100) /
    cores_to_use
  maxmem_gb_per_core <- (memory_to_use_gb /
    cores_to_use) *
    (1 + buffer_percent / 100)

  # Step 4: Set terra options
  terra::terraOptions(
    memfrac = memfrac,
    memmax = maxmem_gb_per_core,
    tempdir = tempdir,
    todisk = TRUE,
    verbose = TRUE
  )

  # Step 5: Print confirmation message
  cat(
    "terraOptions set with memfrac:", memfrac,
    "maxmem:", maxmem_gb_per_core, "GB\n",
    "tempdir:", tempdir, "\n",
    "cores to use:", cores_to_use, "\n"
  )

  # Step 6: Return the number of cores to use
  return(cores_to_use)
}


#' Sample a fraction of rows within blocks
#'
#' This function takes a data frame and samples a specified
#' fraction of rows within each block, defined by a given
#' column. Sampling is done without replacement, so each row
#' can appear at most once in the output.
#'
#' @param data A data frame containing the data to sample from.
#' @param frac Numeric. Fraction of rows to sample from each
#' block (default = 0.05).
#' @param block_column Character. The name of the column in
#' `data` that defines blocks.
#'
#' @return A data frame containing the sampled rows, with
#' approximately `frac` rows per block.
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   block_id = rep(letters[1:4], each = 1000),
#'   value = rnorm(4000)
#' )
#' df_sub <- sample_blocks(
#'   df,
#'   frac = 0.05, block_column = "block_id"
#' )
#' dplyr::count(df_sub, block_id)
sample_blocks <- function(data, frac = 0.05, block_column) {
  # Step 1: Check that the block column exists in the data
  if (!block_column %in% names(data)) {
    stop(glue::glue(
      "Column '{block_column}' not found in data."
    ))
  }

  # Step 2: Group the data by the block column
  result <- data %>%
    dplyr::group_by(.data[[block_column]]) %>%
    # Step 3: Sample the specified fraction of rows within
    # each group
    dplyr::sample_frac(size = frac) %>%
    # Step 4: Remove grouping structure before returning
    dplyr::ungroup()

  # Step 5: Return the sampled data
  return(result)
}

#' Style the Active R File
#'
#' Locate the currently active R source file (in RStudio or from
#' the --file command line argument) and run styler::style_file on
#' it using a tidyverse style with 2-space indent.
#'
#' @return Invisibly returns the path to the styled file, or NULL
#'   if no active file was found.
#' @export
style_active_file <- function() {
  # 1. Helper: find the active file path ----
  get_active_file <- function() {
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
      rstudioapi::getSourceEditorContext()$path
    } else {
      args <- commandArgs(trailingOnly = FALSE)
      file_arg <- grep("--file=", args, value = TRUE)
      if (length(file_arg)) {
        sub("--file=", "", file_arg[1])
      } else {
        NULL
      }
    }
  }

  # 2. Resolve file path
  file_path <- get_active_file()

  # 3. Handle missing file
  if (is.null(file_path)) {
    message("No active file found.")
    return(invisible(NULL))
  }

  # 4. Ensure styler is available
  if (!requireNamespace("styler", quietly = TRUE)) {
    stop("The 'styler' package is required but not installed.")
  }

  # 5. Run styler on the file
  styler::style_file(file_path)

  # 6. Return the path invisibly
  invisible(file_path)
}


#' Generate a Data Frame of Column Classes
#'
#' This function takes a list of data frames and returns a summary
#' data frame showing the classes of each column across all the
#' data frames. Missing columns in some data frames are represented
#' as NA.
#'
#' @param all_data List. A list of data frames.
#' @return A data frame with rows representing the data frames and
#' columns representing the column names from all the data frames. The
#' values are the classes of the columns or NA if the column is
#' missing in a data frame.
#'
#' @example
#' # Example usage:
#' all_data <- list(
#'   df1 = data.frame(a = 1:3, b = letters[1:3]),
#'   df2 = data.frame(a = 4:6, c = c(TRUE, FALSE, TRUE))
#' )
#' column_classes_df <- generate_column_classes(all_data)
#' print(column_classes_df)
#'
summarize_column_classes <- function(all_data) {
  # Step 1: Get column classes for each data frame in all_data
  column_classes <- lapply(all_data, function(df) {
    sapply(df, class)
  })

  # Step 2: Identify all unique column names across the data frames
  all_columns <- unique(unlist(
    lapply(column_classes, names)
  ))

  # Step 3: Create a data frame of column classes
  result <- do.call(rbind, lapply(
    names(column_classes),
    function(df_name) {
      # Step 3.1: Initialize a row with NA for all columns
      row <- setNames(rep(NA, length(all_columns)), all_columns)

      # Step 3.2: Fill in the classes for columns present in the data frame
      col_classes <- column_classes[[df_name]]
      row[names(col_classes)] <- col_classes

      # Step 3.3: Add the data frame name to the row
      c(data_frame = df_name, row)
    }
  ))

  # Step 4: Convert the result to a data frame and return it
  return(as.data.frame(result, stringsAsFactors = FALSE))
}


#' Minimal Theme for Scientific Plots
#'
#' This function creates minimalistic ggplot2 theme for scientific
#' plots in ggplot2. It removes gridlines, simplifies axis
#' formatting, and adds clean, bold titles and labels.
#'
#' @return A ggplot2 theme object.
#'
#' @example
#' # Example usage:
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   labs(title = "Example Plot", x = "Weight", y = "mpg") +
#'   theme_science()
#' print(p)
theme_science <- function() {
  # Create a custom ggplot2 theme
  theme_minimal(base_size = 12, base_family = "sans") +
    theme(
      # Remove gridlines for simplicity
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),

      # Add clean axis lines
      axis.line = element_line(color = "black", linewidth = 0.5),

      # Simplify axis text and titles
      axis.text = element_text(size = 10, color = "black"),
      axis.title = element_text(size = 12, face = "bold"),

      # Customize legend appearance
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),

      # Remove panel background for a clean look
      panel.background = element_rect(fill = "transparent", color = NA),

      # Align titles and subtitles for clarity
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(size = 9, hjust = 1)
    )
}


#' Minimal Theme for Scientific Map Plots
#'
#' This function creates minimalistic ggplot2 theme for scientific
#' plots in ggplot2. It removes gridlines, simplifies axis
#' formatting, and adds clean, bold titles and labels.
#'
#' @return A ggplot2 theme object.
#'
#' @example
#' # Example usage:
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   labs(title = "Example Plot", x = "Weight", y = "mpg") +
#'   theme_science()
#' print(p)
theme_science_map <- function() {
  # Create a custom ggplot2 theme
  theme_minimal(base_size = 12, base_family = "sans") +
    theme(
      # Remove gridlines for simplicity
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),

      # Add clean axis lines
      axis.line = element_line(
        color = "black", linewidth = 0.5
      ),

      # Simplify axis text and titles
      axis.text = element_text(
        size = 10, color = "black"
      ),
      axis.title = element_text(
        size = 12, face = "bold"
      ),

      # Customize legend appearance
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_text(
        size = 10, face = "bold"
      ),
      legend.text = element_text(size = 9),

      # Remove panel background for a clean look
      panel.background = element_rect(
        fill = "transparent", color = "black"
      ),

      # Align titles and subtitles for clarity
      plot.title = element_text(
        size = 14, face = "bold", hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 12, hjust = 0.5
      ),
      plot.caption = element_text(
        size = 9, hjust = 1
      )
    )
}
