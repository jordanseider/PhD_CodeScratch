library(sf)
library(hdf5r)
library(dplyr)
library(future)
library(future.apply)

plan(multisession, workers = 20)

# Define the target directory containing your HDF5 files
target_dir <- "C:/Users/jseider.stu/Downloads/ATL08_007-20260708_221949"

# Generate a list of all .h5 files in the directory
h5_files <- list.files(path = target_dir, pattern = "\\.h5$", full.names = TRUE)

# ---------------------------------------------------------
# Step 1: Define the function to process a single file
# ---------------------------------------------------------
process_atl08_file <- function(file_path) {
  
  # Skip empty NSIDC subset shells (under 50 KB)
  if (file.size(file_path) < 50000) {
    message("Skipping empty shell: ", basename(file_path))
    return(NULL)
  }
  
  # Open the HDF5 file
  h5_file <- H5File$new(file_path, mode = "r")
  
  # Guarantee the file closes when the function finishes, even if an error occurs
  on.exit(h5_file$close_all())
  
  beams <- grep("^gt", names(h5_file), value = TRUE)
  
  # Iterate over each beam using a safe tryCatch block
  beam_data_list <- lapply(beams, function(beam) {
    
    tryCatch({
      # 1. Coordinates
      lon <- h5_file[[paste0(beam, "/land_segments/longitude")]][]
      lat <- h5_file[[paste0(beam, "/land_segments/latitude")]][]
      
      # 2. Canopy Variables
      canopy_h <- h5_file[[paste0(beam, "/land_segments/canopy/h_canopy")]][]
      
      # 3. Terrain Variables
      terrain_h <- h5_file[[paste0(beam, "/land_segments/terrain/h_te_best_fit")]][]
      
      # Compile into a dataframe
      df <- data.frame(
        source_file = basename(file_path),
        beam = beam,
        lon = as.numeric(lon),
        lat = as.numeric(lat),
        canopy_height_m = as.numeric(canopy_h),
        terrain_height_m = as.numeric(terrain_h)
      )
      
      return(df)
      
    }, error = function(e) {
      # If any data group is missing or corrupted, silently skip this beam
      return(NULL)
    })
  })
}
# ---------------------------------------------------------
# Step 2: Run the loop across all files and combine
# ---------------------------------------------------------
message(sprintf("Processing %d files across multiple cores...", length(h5_files)))

# lapply runs the function over the list of files; bind_rows stacks them
all_data_list <- future_lapply(h5_files, process_atl08_file, future.seed = TRUE)
atl08_master_df <- bind_rows(all_data_list)

plan(sequential)

# ---------------------------------------------------------
# Step 3: Clean data and convert to spatial object
# ---------------------------------------------------------

# Handle NoData values
atl08_master_df$canopy_height_m[atl08_master_df$canopy_height_m > 1000]     <- NA
atl08_master_df$terrain_height_m[atl08_master_df$terrain_height_m > 100000] <- NA

# Remove NA rows to reduce file size
atl08_clean <- atl08_master_df %>% filter(!is.na(canopy_height_m))

# Convert to sf object
atl08_sf <- st_as_sf(atl08_clean, 
                     coords = c("lon", "lat"), 
                     crs = 4326)

# ---------------------------------------------------------
# Step 4: Write to GeoPackage
# ---------------------------------------------------------
output_gpkg <- file.path(target_dir, "ATL08_Combined_Output.gpkg")

# append = FALSE overwrites the file if it already exists
st_write(atl08_sf, output_gpkg, append = FALSE)

message("Processing complete. GeoPackage saved to: ", output_gpkg)
