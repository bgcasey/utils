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
#' @export
snake_case <- function(x) {
    x <- gsub("[^A-Za-z0-9_ ]", "", x) # Remove special characters
    x <- gsub(" ", "_", x) # Replace spaces with underscores
    x <- gsub("([a-z])([A-Z])", "\\1_\\2", x) # Add _ between letters
    # x <- gsub("([0-9])([A-Za-z])", "\\1_\\2", x)  # Add _ between numbers
    # x <- gsub("([A-Za-z])([0-9])", "\\1_\\2", x)  # Add _ between letters and numbers
    x <- gsub("__+", "_", x) # Replace multiple underscores with a single _
    tolower(x) # Convert to lowercase
}
