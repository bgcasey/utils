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
#'     gbm_data        = gbm_data,
#'     chosen_response = "cirsium_arvense",
#'     response_vars   = response_vars,
#'     reorder         = TRUE
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
