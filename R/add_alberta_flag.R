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
        dat_sf, ab,
        sparse = FALSE
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
