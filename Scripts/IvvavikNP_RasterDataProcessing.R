# =============================================================================
# Environmental Covariate Data Processing - Ivvavik National Park
# Jordan Seider
# =============================================================================

# -----------------------------------------------------------------------------
# Environment Setup & Configuration
# -----------------------------------------------------------------------------

library(terra)
library(tidyterra)
library(rgeomorphon)
library(fasterRaster)

# Initialize GRASS GIS for fasterRaster
faster(grassDir = "C:/Program Files/GRASS GIS 8.4")

# Global Terra Options
terraOptions(
  memfrac = 0.8, # max proportion of memory allowed for use by terra
  steps = 4, # fewer, larger chunks reduces I/O overhead for large rasters
  threads = 26, # i9-13950HX has 32 threads; leave a few free for the OS
  tempdir = "C:/Users/jseider.stu/AppData/Local/Temp"
)

# Global rgeomorphon Options
Sys.setenv(R_RGEOMORPHON_N_WORKERS = 26, R_RGEOMORPHON_MEM_SCALE_NEED = 5)

setwd("C:/Users/jseider.stu")

# -----------------------------------------------------------------------------
# Generate Raster Template from DEM
# -----------------------------------------------------------------------------

# If adding new variables, use these lines to create the template for resampling
template_10m <- rast(
  "./Sync/Data/ArcticDEM/IvvavikNP/ArcticDEM_IvvavikNP_10m.tif"
)[[1]]

# -----------------------------------------------------------------------------
# Study Area Boundary
# -----------------------------------------------------------------------------

ivvavik <- vect(
  "./Sync/Data/ParksCanada/IvvavikNationalPark/IvvavikNationalPark.shp"
) |>
  project("EPSG:32607") # WGS 84 / UTM Zone 7N

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

# Write a compressed, tiled GeoTIFF (use `predictor = 2` if categorical)
write_tif <- function(r, path, predictor = 3, overwrite = TRUE, ...) {
  writeRaster(
    r,
    path,
    gdal = c(
      "COMPRESS=DEFLATE",
      paste0("PREDICTOR=", predictor),
      "TILED=YES",
      "BLOCKXSIZE=512",
      "BLOCKYSIZE=512"
    ),
    overwrite = overwrite,
    ...
  )
}

# Average DayMet daily layers across years by DOY
process_daymet <- function(
  pattern,
  daymet_dir,
  years,
  target,
  boundary,
  var_prefix
) {
  message(paste0("Processing DayMet: ", pattern, "..."))

  # Locate all target files across all years using a single regex pattern
  files <- list.files(
    daymet_dir,
    pattern = paste0(
      pattern,
      ".*(",
      paste(years, collapse = "|"),
      ").*\\.tif$"
    ),
    full.names = TRUE
  )

  # Align the park boundary to the raw DayMet CRS
  bnd_raw <- project(boundary, crs(rast(files[1])))

  # Load the pointers, crop immediately, compute DOY means, rename, and project to 10m
  rast(files) |>
    crop(bnd_raw, mask = TRUE) |>
    tapp(index = rep(1:365, times = length(years)), fun = mean, na.rm = TRUE) |>
    setNames(paste0(var_prefix, sprintf("%03d", 1:365))) |>
    project(target, method = "bilinear") |>
    mask(boundary)
}

# -----------------------------------------------------------------------------
# 10-Metre DEM
# -----------------------------------------------------------------------------

path_10m <- "./Sync/Data/ArcticDEM/IvvavikNP/ArcticDEM_IvvavikNP_10m.tif"

dem_10m <- rast("./Sync/Data/ArcticDEM/202505_ArcticDEM_mosaic_10m.tif")[[
  "dem"
]] |>
  (\(r) crop(r, project(ivvavik, crs(r))))() |>
  (\(r) {
    # Generate template dynamically inside the pipe
    tmpl <- rast(
      crs = crs(ivvavik),
      extent = ext(project(r, ivvavik)),
      res = 10
    )

    # Calculate terrain and project all at once
    c(r, terrain(r, c("slope", "aspect", "TPI"), unit = "degrees")) |>
      project(tmpl, method = "bilinear")
  })() |>
  setNames(c("dem_10m", "slope_10m", "aspect_10m", "TPI_10m")) |>
  (\(r) {
    # Append calculated aspect components
    r$aspect_ns_10m <- cos(r[["aspect_10m"]] * pi / 180)
    r$aspect_ew_10m <- sin(r[["aspect_10m"]] * pi / 180)
    r
  })() |>
  crop(ivvavik, mask = TRUE)

write_tif(dem_10m, path_10m)

# Extract template for downstream resampling
template_10m <- dem_10m[[1]]

# -----------------------------------------------------------------------------
# Topographic Wetness Index (TWI) - via GRASS / fasterRaster
# -----------------------------------------------------------------------------

path_twi <- "./Sync/Data/ArcticDEM/IvvavikNP/ArcticDEM_IvvavikNP_twi_10m.tif"

twi_10m <- fast(path_10m)$dem_10m |>
  wetness() |>
  rast() |>
  project(template_10m, method = "bilinear") |>
  setNames("twi_10m")

write_tif(twi_10m, path_twi)

# -----------------------------------------------------------------------------
# Geomorphons
# -----------------------------------------------------------------------------

path_geomorphons <- "./Sync/Data/ArcticDEM/IvvavikNP/geomorphons_0_40.tif"

geomorphons_10m <- rgeomorphon::geomorphons(
  elevation = dem_10m$dem_10m,
  forms = "forms10",
  search = 40, # 400 m
  skip = 0, # 0 m
  flat_angle_deg = 1
) |>
  project(template_10m, method = "near")

write_tif(geomorphons_10m, path_geomorphons, predictor = 2, datatype = "INT1U")


# --- CLEANUP BLOCK ---
rm(dem_10m, twi_10m, geomorphons_10m)
gc()
tmpFiles(remove = TRUE)


# -----------------------------------------------------------------------------
# Phenology - MS-LSP EVI Area & Onset of Greenness (2016-2023)
# -----------------------------------------------------------------------------

# --- EVI Area ---
eviarea <- rast(
  "E:/PhenologyDataFromSeamore/Processed/phenology_EVIarea.tif"
) |>
  (\(r) crop(r, project(ivvavik, crs(r)), mask = TRUE))()
eviarea <- project(eviarea, template_10m, method = "bilinear")
eviarea <- mask(eviarea, ivvavik)
eviarea <- setNames(eviarea, paste0("eviarea_", 2016:2023))

write_tif(eviarea, "./Sync/Data/Phenology/MS-LSP/eviarea_ivvavik.tif")

# --- Onset of Greenness (OGI) ---
ogi <- rast("E:/PhenologyDataFromSeamore/Processed/phenology_OGI.tif")
ogi <- crop(ogi, project(ivvavik, crs(ogi)), mask = TRUE)
ogi <- project(ogi, template_10m, method = "bilinear")
ogi <- mask(ogi, ivvavik)
ogi <- setNames(ogi, paste0("ogi_", 2016:2023))

# Check if all values are whole numbers
all(values(eviarea) == floor(values(eviarea)), na.rm = TRUE)
# If TRUE, use 'predictor = 2' for float values

write_tif(ogi, "./Sync/Data/Phenology/MS-LSP/ogi_ivvavik.tif")

# -----------------------------------------------------------------------------
# Plant Functional Types - Macander Top Cover (2015)
# -----------------------------------------------------------------------------

path_pft <- "./Sync/Data/AnnualPFT_ABoVE_Macander/IvvavikNP_2015_TopCover.tif"

tc_2015 <- rast(list.files(
  "./Sync/Data/AnnualPFT_ABoVE_Macander/2015",
  full.names = TRUE
))
tc_2015 <- crop(tc_2015, project(ivvavik, crs(tc_2015)), mask = TRUE)
tc_2015 <- project(tc_2015, template_10m, method = "bilinear")
tc_2015 <- mask(tc_2015, ivvavik)
tc_2015 <- setNames(
  tc_2015,
  c(
    "broadleaf_tree_2015",
    "conifer_tree_2015",
    "deciduous_shrub_2015",
    "evergreen_shrub_2015",
    "forb_2015",
    "graminoid_2015",
    "lichen_2015"
  )
)

write_tif(tc_2015, path_pft)


# --- CLEANUP BLOCK ---
rm(eviarea, ogi, tc_2015)
gc()
tmpFiles(remove = TRUE)


# -----------------------------------------------------------------------------
# Climate - DayMet Daily Data (2016-2023)
# -----------------------------------------------------------------------------

daymet_dir <- "E:/DayMet/Ivvavik/"
years <- 2016:2023

# --- Day Length (sec/day) ---

dayl <- file.path(daymet_dir, "Ivvavik_daymet_v4_daily_na_dayl_2016.tif") |>
  rast()
dayl <- project(dayl, template_10m, method = "bilinear")
dayl <- mask(dayl, ivvavik)

write_tif(dayl, "./Sync/Data/Climate/Ivvavik_dayl_2016.tif")

# --- Snow Water Equivalent (SWE) in kg/m2 ---

mean_swe <- process_daymet(
  "swe",
  daymet_dir,
  years,
  template_10m,
  ivvavik,
  "avg_swe_doy_"
)

mean_swe[[100:120]] |>
  mean(na.rm = TRUE) |>
  setNames("mean_swe_100_120") |>
  write_tif("./Sync/Data/Climate/meanSWE_doy100_120.tif")
mean_swe[[121:150]] |>
  mean(na.rm = TRUE) |>
  setNames("mean_swe_121_150") |>
  write_tif("./Sync/Data/Climate/meanSWE_doy121_150.tif")


# --- CLEANUP BLOCK ---
rm(dayl, mean_swe)
gc()
tmpFiles(remove = TRUE)


# --- Temperature and Derived Metrics ---

mean_tmax <- process_daymet(
  "tmax",
  daymet_dir,
  years,
  template_10m,
  ivvavik,
  "avg_tmax_doy_"
)
mean_tmin <- process_daymet(
  "tmin",
  daymet_dir,
  years,
  template_10m,
  ivvavik,
  "avg_tmin_doy_"
)

# Monthly Mean Maximum Temperature (March-June)
tmax_subset <- mean_tmax[[60:181]]
month_groups <- c(
  rep("March_Mean_Tmax", 31),
  rep("April_Mean_Tmax", 30),
  rep("May_Mean_Tmax", 31),
  rep("June_Mean_Tmax", 30)
)

monthly_tmax <- tapp(
  tmax_subset,
  index = month_groups,
  fun = mean,
  na.rm = TRUE
)

write_tif(monthly_tmax, "./Sync/Data/Climate/Ivvavik_monthly_mean_tmax.tif")


# --- CLEANUP BLOCK ---
# We keep mean_tmax and mean_tmin for the GDD calculation, but
# we can destroy the day length, SWE, and subsets.
rm(tmax_subset, monthly_tmax)
gc()
tmpFiles(remove = TRUE)


# Growing Degree Days (GDD) up to DOY 150 (approx. start of green up)

# This just groups them in R; it takes up less disk space
climate_stack <- c(mean_tmax[[1:150]], mean_tmin[[1:150]])

# Define a function to process the math purely in memory
gdd_matrix_calc <- function(m, base_temp = 0) {
  # 'terra' feeds chunks of pixels to this function as a matrix 'm'
  # Rows = pixels, Columns = 300 layers (150 tmax + 150 tmin)

  # Split the matrix back into tmax and tmin components
  # Columns 1 through 150: These are your mean_tmax values for days 1 to 150.
  # Columns 151 through 300: These are your mean_tmin values for days 1 to 150.
  tmax_m <- m[, 1:150]
  tmin_m <- m[, 151:300]

  # Calculate daily mean
  tmean_m <- (tmax_m + tmin_m) / 2

  # Subtract base temp to get GDD
  gdd_m <- tmean_m - base_temp

  # Clamp: replace any negative values with 0
  gdd_m[gdd_m < 0] <- 0

  # Sum across the 150 days (columns) to get total GDD for each pixel (row)
  rowSums(gdd_m, na.rm = TRUE)
}

# Apply the function block-by-block directly to the output file
gdd_doy150 <- app(climate_stack, fun = gdd_matrix_calc, base_temp = 0)

write_tif(gdd_doy150, "./Sync/Data/Climate/Ivvavik_gdd_doy150.tif")
