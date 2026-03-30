## Phenocam data processing

library(dplyr)
library(lubridate)
library(sf)
library(terra)
library(ggplot2)
library(gstat) # for spatial interpolation

# Plants_visible     = Plants first visible through snow
# SnowFree_90        = Snow free melt date (>90% plot free of snow)
# SnowFree_100       = First 100% snow-free day
# SnowReturn_day1    = First snow return day - end of season
# SnowReturn_50      = 50% snow coverage - end of season
# SnowReturn100      = 100% snow coverage - end of season
# Plants_FirstBurst  = First leaf bud burst
# Plants_50green     = 50% leaves green
# Plants_100green    = 100% leaves green
# Plants_FirstYellow = First yellow leaf
# Plants_50yellow    = 50% yellow leaves
# Plants_100yellow   = 100% yellow leaves

setwd("C:/Users/jseider.stu/Sync/Data/Phenology")

qhi <- st_read(
  "C:/Users/jseider.stu/Sync/Data/OpenYukon/ProtectedAreas/Parks and Protected Areas (250k)/YTParksProtectedAreas250K"
) %>%
  st_transform(crs = 4326) %>%
  filter(area_name == "Herschel Island - Qikiqtaruk Territorial Park") %>%
  st_geometry()

pheno_raw <- read.csv("./QHI_Phenocam_2023-24_CSV.csv") %>%
  select(-NOTES) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  rename(
    plot = PLOT,
    year = Year,
    site = Site,

    Plants_visible = Plants.first.visible.through.snow,
    SnowFree_90 = Snow.Free.Melt.Date...90..plot.free.of.snow.,
    SnowFree_100 = First.100..snow.free.day,
    Plants_FirstBurst = First.leaf.bud.burst,
    Plants_50green = X50..Leaves.Green,
    Plants_100green = X100..Leaves.Green,
    Plants_FirstYellow = First.yellow.leaf,
    Plants_50yellow = X50..Leaves.Yellow,
    Plants_100yellow = X100..Leaves.Yellow,
    SnowReturn_day1 = First.snow.return.day...end.of.season,
    SnowReturn_50 = X50..snow.coverge...end.of.season,
    SnowReturn_100 = X100..snow.coverage...end.of.season
  )

date_columns <- pheno_raw %>%
  st_drop_geometry() %>%
  select(!c(plot, year, site)) %>%
  names()

pheno_doy <- pheno_raw %>%
  mutate(across(all_of(date_columns), ~ yday(dmy(.)))) %>%
  select(
    plot,
    year,
    Plants_visible,
    SnowFree_90,
    SnowFree_100,
    Plants_FirstBurst,
    Plants_50green,
    Plants_100green,
    Plants_FirstYellow,
    Plants_50yellow,
    Plants_100yellow,
    SnowReturn_day1,
    SnowReturn_50,
    SnowReturn_100
  )

ggplot() +
  geom_sf(data = qhi, fill = NA) +
  geom_sf(data = pheno_doy, size = 1) +
  theme_minimal()

dem <- rast("./QHI_ArcticDEM_2m.tif")

data.df <- terra::extract(dem, pheno_doy, xy = TRUE, bind = TRUE)

tomst <- read.csv("./tomst_wide_processed.csv")
data.df <- merge(
  data.df,
  tomst,
  by.x = c("plot", "year"),
  by.y = c("site", "year")
)
data.df$site_id <- NULL
