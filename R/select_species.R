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
#' @examples
#' \dontrun{
#' # Example dataset
#' invsp_wide <- data.frame(
#'     site = c("1001", "1002"),
#'     year = c(2020, 2021),
#'     obs_date = as.Date(c("2020-06-15", "2021-07-20")),
#'     latitude = c(54.12, 54.45),
#'     longitude = c(-113.5, -114.2),
#'     ranunculus_acris = c(1, 0)
#' )
#'
#' # Selecting a species
#' selected_species_df <- select_species(invsp_wide, "Ranunculus acris")
#'
#' # View result
#' print(selected_species_df)
#' }
#' @export
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
