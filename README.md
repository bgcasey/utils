# utils

![In Development](https://img.shields.io/badge/Status-In%20Development-yellow) 
![Languages](https://img.shields.io/badge/Languages-R-blue)


A library of utility functions to support data processing, spatial workflows, modelling, and general analysis across projects.

## Installation

You can install the package directly from GitHub:

```r
devtools::install_github("bgcasey/utilR")
```

## Functions

The package includes the following utility functions:

### Spatial Functions
- `add_alberta_flag()` - Add Alberta flag to a site data frame using provincial boundaries
- `extract_by_year()` - Extract raster values by year from spatial features
- `parallel_extract_directory()` - Parallel extraction of raster data stored in a directory

### Time & Processing Functions
- `estimate_processing_time()` - Estimate total processing time based on subset
- `format_time_diff()` - Format time difference as days, hours, minutes, and seconds

### File I/O Functions
- `load_most_recent_data()` - Load the most recent .RData file dynamically
- `load_rdata_files()` - Load .RData files into a list
- `read_tifs_to_list()` - Read TIF files into a named list
- `read_tifs_to_multiband()` - Read TIF files into a multiband raster object

### Data Manipulation Functions
- `select_one_response()` - Select a single response column from data
- `select_species()` - Select a specific species column
- `sample_blocks()` - Sample a fraction of rows within blocks
- `snake_case()` - Convert strings to snake_case
- `summarize_column_classes()` - Generate a data frame of column classes

### Configuration Functions
- `set_terra_options()` - Set Terra options with buffer for memory management

### Plotting & Styling Functions
- `theme_science()` - Minimal theme for scientific plots
- `theme_science_map()` - Minimal theme for scientific map plots
- `style_active_file()` - Style the active R file using styler

## Usage

```r
library(utilR)

# Example: Convert strings to snake_case
snake_case(c("Normal 1991_2020 AHM", "Normal 1991_2020 bFFP"))

# Example: Format time difference
start_time <- Sys.time()
Sys.sleep(5)
end_time <- Sys.time()
format_time_diff(start_time, end_time)

# Example: Use custom ggplot2 theme
library(ggplot2)
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  labs(title = "Example Plot", x = "Weight", y = "MPG") +
  theme_science()
```

## Dependencies

The package requires the following R packages:
- dplyr
- sf
- terra
- rnaturalearth
- ggplot2
- glue
- parallel

## License

MIT 
