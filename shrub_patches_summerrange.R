library(terra)
library(landscapemetrics)
library(dplyr)

# Optimize 'terra' processing
terraOptions(memfrac = 0.8)

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
shrub_patches <- patches(tall_decid, directions = 8)

# Calculate the area of every single cell in square kilometers (km)
cell_area_km2 <- cellSize(shrub_patches, unit = "km")

# Sum the cell areas, grouped by their unique patch ID
patch_areas <- zonal(cell_area_km2, shrub_patches, fun = "sum")
colnames(patch_areas) <- c("patch_id", "area_km2")

# Filter for patches strictly greater than 0.25 km^2
large_patch_ids <- patch_areas$patch_id[patch_areas$area_km2 > 0.25]

# Mask the patches raster to only keep the large ones
large_patches_raster <- match(shrub_patches, large_patch_ids)

# Convert to categorical for 'landscapemetrics' package analyses
# 1 = Tall Deciduous Shrub Patch, NA = everything else
telldecid_patches <- ifel(!is.na(large_patches_raster), 1, NA)
