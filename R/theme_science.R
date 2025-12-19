#' Minimal Theme for Scientific Plots
#'
#' This function creates minimalistic ggplot2 theme for scientific
#' plots in ggplot2. It removes gridlines, simplifies axis
#' formatting, and adds clean, bold titles and labels.
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' # Example usage:
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point() +
#'     labs(title = "Example Plot", x = "Weight", y = "mpg") +
#'     theme_science()
#' print(p)
#' @importFrom ggplot2 theme_minimal theme element_blank element_line element_text element_rect
#' @export
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
