### Downloading and Merging Arctic DEM Tiles
### Jordan Seider
### 2025-06-13

## NOTES ##

# Should use the fastest internet connection you can for best download speeds. 
# Should have computer with enough storage to download LARGE DEM files prior to aggregation.

# The end of this script has a step to aggregate to 30 m. This is recommended to reduce file size
# but may not be needed if you want to use the 2 m resolution.

# Use the ArcticDEM tile index (https://www.pgc.umn.edu/data/arcticdem/) and find the intersection
# with your study area to determine which tiles to download (ie. use the feature selection to select 
# only intersecting tiles). Save the attribute table of only the selected tiles.

# Data frame should have the following headers: "dem_id", "tile", "fileurl", "bulk download code", though really
# only the "name" and fileurl" column is used to download the tiles.


setwd("")
options(timeout = 999999) # required to prevent downloads from stopping due to timeouts

library(sf)
library(dplyr)

arcticDEM_tiles <- st_read("") %>% st_transform("EPSG:4326")
studyarea       <- st_read("") %>% st_transform("EPSG:4326")

files <- st_intersection(arcticDEM_tiles, studyarea) %>%
  select(dem_id, tile, fileurl)

## Lines below required if downloading data other than raw. For some reason, when trying to download the 
## 10m resolution data, the URL was written incorrectly (showed the 5m instead of 10m; also some other 
## differences). Change file names to correctly download the data. Check the URL from the CSV that it 
## correctly downloads from personal computer web browser to ensure correct link.

# files$dem_id <- gsub(pattern = "....5m_v2.0", "10m_v3.0", files$dem_id)

# files$fileurl <- gsub(pattern = "http:", "https:", files$fileurl)
# files$fileurl <- gsub(pattern = "v2.0/", "v3.0/10m/", files$fileurl)
# files$fileurl <- gsub(pattern = "....5m_v2.0", "10m_v3.0", files$fileurl)

for(i in 1:nrow(files)){
  
  # Define the expected output file to check if tile has already been downloaded
  output_file <- file.path(getwd(), "ArcticDEM_tiles", paste0(files$dem_id[i], "_dem.tif"))
  
  # Skip iteration if file already exists
  if(file.exists(output_file)){
    message(paste("File aready exists:", output_file, "- Skipping download."))
    next
  }
  
  td <- tempdir()
  tf <- tempfile(tmpdir = td, 
                 fileext = ".tar.gz")
  
  url = as.character(files$fileurl[i])
  download.file(url, tf)
  
  untar(tf, 
        files = paste0(files$dem_id[i], "_dem.tif"),
        exdir = paste0(getwd(), "/ArcticDEM_tiles"))
  
  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE)
  
}

# Merge tiles
library(terra)

v <- vrt(list.files("./ArcticDEM_tiles", full.names = TRUE), "temp_vrt.vrt", overwrite = TRUE) # merges rasters
raster <- c(v, 
            terrain(x = v, 
                    v = c("slope", "aspect", "TPI"))) #calculate slope, aspect, and topographic position
names(raster) <- c("dem", "slope", "aspect", "TPI")

writeRaster(raster, "./202505_ArcticDEM_mosiac_2m.tif")

raster10 <- aggregate(raster, fact = 5) # aggregates from 2 m to 10 m
writeRaster(raster10, "./202505_ArcticDEM_mosiac_10m.tif")

raster30 <- aggregate(raster, fact = 15)
writeRaster(raster30, "./202505_ArcticDEM_mosaic_30m.tif")
