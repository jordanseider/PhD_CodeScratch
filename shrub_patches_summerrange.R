library(terra)
library(landscapemetrics)
library(dplyr)
library(foreach)
library(doParallel)
library(tidyr)
library(sf)

# Optimize 'terra' processing
# memfrac determines the proportion of computer memory 'terra' is allowed to use before writing temp files to slower SSD
terraOptions(memfrac = 0.8)

setwd("C:/Users/jseider.stu/Sync")

# Load Macander PFT (deciduous shrubs) for 2015 (closest alignment with ABoVE land cover data)
decid_2015 <- rast(
  "Data/AnnualPFT_ABoVE_Macander/ABoVE_PFT_Top_Cover_DeciduousShrub_2015.tif"
)

# Define which ABoVE land cover tiles to import
data_dir <- "C:/Users/jseider.stu/Sync/Data/ABoVE_Data/Annual_Landcover_ABoVE_1691/data"
lc_tiles <- c(
  "Bh08v3",
  "Bh08v04",
  "Bh09v03",
  "Bh09v04",
  "Bh09v05",
  "Bh10v04",
  "Bh10v05"
) # tiles overlapping PCH summer range

# Get the file paths for the correct tiles
files <- list.files(data_dir, pattern = "\\.tif$", full.names = TRUE)
target_files <- files[
  grepl(paste(lc_tiles, collapse = "|"), files) &
    !grepl("simplified", files, ignore.case = TRUE)
]

# Convert tiles to raster objects and merge into one object
# Selecting only band 31 (year 2014)
merged_lc <- merge(sprc(lapply(target_files, function(x) rast(x)[[31]]))) %>%
  project(decid_2015, method = "near")
names(merged_lc) <- "landcover_2014"

# Reclassify ABoVE dataset to show only Tall Shrub class (6)
# 6 = Tall Shrub = Area dominated by woody vegetation between 50cm and 3m tall and shrub canopy coverage >60% coverage. Typically, but not always, deciduous phenological habit.
above_tallshrub <- classify(merged_lc, cbind(6, 1), others = NA)

# Create combined dataset to delineate likely tall deciduous shrubs
threshold = 50 # minimum percent cover from Macander to class pixel as deciduous shrub

tall_decid <- mask(decid_2015, above_tallshrub)
tall_decid[tall_decid < threshold] <- NA

# Define patches
# Group touching pixels into unique patches (using 8-way connectivity)
## If this takes too long (it does, up to 8 hours to process), change directions to 4.
# This uses the rook's case and does not count diagonal cells as a continued part of the patch.
# Could you justify this by considering that a tile is 30m*30m and diagonal cells are further, ecologically, from the centre ?
shrub_patches <- patches(tall_decid, directions = 8)
# Saved a local copy of 8 neighbour output to: "~/Sync/Data/Shrubs/shrub_patches.tif"

# Calculate the area of every single cell in square kilometers (km2)
# We know that the pixels are 30*30m but this accounts for the spherical shape of Earth to get a better calculation of exact cell area (note most cells are very close to 900 square metres)
cell_area_km2 <- cellSize(shrub_patches, unit = "km")

# Sum the cell areas, grouped by their unique patch ID
patch_areas <- zonal(cell_area_km2, shrub_patches, fun = "sum")
colnames(patch_areas) <- c("patch_id", "area_km2")

# Filter for patches strictly greater than 0.25 km^2
large_patch_ids <- patch_areas$patch_id[patch_areas$area_km2 > 0.25]

# Mask the patches raster but keep the ORIGINAL patch IDs intact
large_patches_raster <- ifel(
  shrub_patches %in% large_patch_ids,
  shrub_patches,
  NA
)

# Convert the filtered raster into spatial polygons to use as cookie cutters for processing with 'landscapemetrics'
large_patches_poly <- as.polygons(large_patches_raster)
names(large_patches_poly) <- "patch_id"

# Convert terra SpatVector into a sf dataframe to avoid errors associated with parallelizing terra objects
patches_sf <- st_as_sf(large_patches_poly)

# Wrap the raster so it survives the background transfer to multiple cores (for parallel processing)
wrapped_raster <- wrap(large_patches_raster)

use_cores <- 12
cl <- makeCluster(use_cores)
registerDoParallel(cl)

cat("Starting thread-safe parallel processing across", use_cores, "cores...\n")

# Iterating over the safe sf dataframe
final_lsm_data <- foreach(
  i = 1:nrow(patches_sf),
  .combine = bind_rows,
  .packages = c("terra", "landscapemetrics", "dplyr", "sf")
) %dopar%
  {
    # A. Unpack the massive raster
    worker_raster <- unwrap(wrapped_raster)

    # B. Subset the sf dataframe for one (i) patch
    single_poly_sf <- patches_sf[i, ]
    current_id <- single_poly_sf$patch_id

    # C. Convert JUST this one row back to a terra SpatVector to crop the worker_raster object
    single_poly_terra <- vect(single_poly_sf)

    # D. CROP: Cut the worker_raster object to just the one patch (i)
    tiny_rast <- crop(worker_raster, single_poly_terra)

    # E. MASK: Make tiny_rast binary where 1 is the patch and NA is not the patch
    tiny_rast <- ifel(tiny_rast == current_id, 1, NA)

    # F. CALCULATE: Calculate patch metric for single patch (i)
    # Using level = "class", but this is equivalent to calculating metrics on the "patch" scale since we are only looking at one patch
    patch_metrics <- calculate_lsm(
      tiny_rast,
      level = "class",
      metric = c("area", "core", "shape", "perim"),
      progress = FALSE
    )

    # G. ATTACH ID
    patch_metrics <- patch_metrics %>% mutate(patch_id = current_id)

    return(patch_metrics)
  }

stopCluster(cl)
cat("Parallel processing complete!\n")

clean_patch_metrics <- final_lsm_data %>%
  # Keep only the rows containing the actual values
  # 'mn' = mean, since we are only calculating matrics on one patch, not an entire class with many patches, we do not have values for std dev or cv
  filter(grepl("_mn", metric)) %>%
  # Drop the useless landscapemetrics structural columns
  select(patch_id, metric, value) %>%
  # Clean the metric names (e.g., turn "area_mn" into just "area")
  mutate(metric = gsub("_mn", "", metric)) %>%
  # Pivot the table so each metric gets its own column
  pivot_wider(names_from = metric, values_from = value) %>%
  # Rename columns to include units
  rename(area_ha = area, core_ha = core, shape_index = shape) # Ratio index where 1 is highly compact, and >1 is increasingly complex/convoluted

# Merge the attribute table onto your spatial polygons
large_patches_poly <- merge(
  large_patches_poly,
  clean_patch_metrics,
  by = "patch_id"
) %>%
  project("EPSG:3579")
