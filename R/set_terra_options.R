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
#' \dontrun{
#' cores <- set_terra_options(
#'     total_memory_gb = 128,
#'     total_cores = 20,
#'     resource_percent = 50,
#'     priority = "default",
#'     buffer_percent = 10,
#'     tempdir = "D:/r_temp/RtmpWSPfnw"
#' )
#' print(cores)
#' }
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
