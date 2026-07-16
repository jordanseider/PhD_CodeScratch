library(readr)
library(terra)
library(dplyr)
library(furrr)

setwd("E:/NASA_LVIS")

# 1. Set up parallel processing
# Reserving a few threads so your OS remains fully responsive
plan(multisession, workers = 28) 

# Use regex to find files ending strictly in .txt or .TXT
file_paths <- list.files("./LVISF2_1-20260708_181445/", 
                         full.names = TRUE, 
                         pattern = "\\.txt$", 
                         ignore.case = TRUE)

# 2. Define the parsing function (same as before)
read_lvis_safe <- function(path) {
  raw_header <- read_lines(path, skip = 19, n_max = 1)
  clean_header <- gsub("^#\\s*", "", raw_header)
  lvis_colnames <- strsplit(clean_header, "\\s+")[[1]]
  
  lvis_data <- read_table(path, 
                          skip = 20, 
                          col_names = lvis_colnames,
                          show_col_types = FALSE) |>
    mutate(GLON = GLON - 360)
  return(lvis_data)
}

# 3. Execute in parallel
# future_map_dfr acts exactly like map_dfr, but infinitely faster here
combined_lvis_data <- future_map_dfr(file_paths, read_lvis_safe) |>
  vect(
    geom = c("GLON", "GLAT"), 
    keepgeom = TRUE,
    crs = "EPSG:4326"                  
  ) |>
  project("EPSG:3579")

rast_template_10m <- rast(combined_lvis_data, resolution = 30)

lvis_raster <- rasterize(
  x = combined_lvis_data,
  y = rast_template_10m,
  field = "RH100", 
  fun = "max", 
  background = NA, 
  filename = "./LVISF2_1-20260708_181445/LVIS_F2_RH100.tif", 
  overwrite = TRUE
  )
