## Download historic air photos in northern Yukon

library(sf)
library(dplyr)
library(exiftoolr)
library(curl)

setwd("C:/Users/jseider.stu/OneDrive - UBC/Data/OpenYukon/AirPhotos")

sites <- st_read("C:/Users/jseider.stu/OneDrive - UBC/Field/2025_FieldSeason/Data/CaribouCams_SHP/CaribouCam_Locations_2025.shp") 
airphotos <- st_read("C:/Users/jseider.stu/OneDrive - UBC/Data/OpenYukon/AirPhotos/Air_Photo_Locations.gdb", layer = "Air_Photo_Locations") %>%
  st_transform(st_crs(sites))

data <- airphotos %>% st_filter(st_buffer(sites, 3000)) %>%
  select(AIRPHOTO_ID, ROLL_NUMBER, PHOTO_NUMBER, YEAR, 
         SCALE, LIBRARY_CATALOG_LINK, AIRPHOTO_FTP_LINK) %>%
  mutate(lon = st_coordinates(.)[, "X"], 
         lat = st_coordinates(.)[, "Y"]) %>% st_drop_geometry()

## Download available digitized air photos
for(i in 1:nrow(data)){
  
  url <- as.character(data$AIRPHOTO_FTP_LINK[i])
  photoname <- paste0("YukonAirPhoto_",
                      data$ROLL_NUMBER[i], "_",
                      data$PHOTO_NUMBER[i], "_",
                      data$YEAR[i], ".jpg")
  destination <- file.path(paste0(getwd(), "/", photoname))
  
  # Skip iteration if file already exists
  if(file.exists(destination)){
    warning(paste("File already exists:", destination, "- Skipping download."),
            immediate. = TRUE)
    next
  }
  
  # Skip iteration if no airphoto is available to download
  if(is.na(url)){
    warning(paste("Digitized air photo does not exist. Check Library Catalogue Link instead:", data$LIBRARY_CATALOG_LINK[i], "- Skipping download."))
    next
  }
    
  # Download with Retry Logic
  message(paste("Attempting to download:", photoname))
  
  download_successful <- FALSE
  max_retries <- 3 # We will try to download each file up to 3 times
  
  for (attempt in 1:max_retries) {
    tryCatch({
      
      h <- new_handle()
      handle_setheaders(
        h,
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
        "Referer" = "https://open.yukon.ca/" # A plausible referring site
      )
      
      curl_download(url, destination, quiet = FALSE, handle = h)
      
      # If the above line completes without error, the download was successful.
      download_successful <- TRUE
      message("Download successful.")
      break # Exit the retry loop
      
    }, error = function(e) {
      message(paste("Attempt", attempt, "failed for", photoname, "- Error:", e$message))
      if (attempt < max_retries) {
        Sys.sleep(5) # Wait for 5 seconds before the next attempt
      }
    })
    if(download_successful) break
  }
  
  # Construct the arguments for ExifTool
  args_geotagging <- c(paste0("-GPSLatitude=", data$lat[i]),
                       paste0("-GPSLongitude=", abs(data$lon[i])), #use absolute to remove negative value
                       paste0("-GPSLatitudeRef=N"),
                       paste0("-GPSLongitudeRef=W"),
                       "-GPSVersionID=2.3.0.0",
                       "-GPSMapDatum=WGS-84",                       # For legacy compatibility
                       "-overwrite_original"                        # To avoid creating backup files
  )
  
  exif_call(args = args_geotagging,
            path = destination)
}

