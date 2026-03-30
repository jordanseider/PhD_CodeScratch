library(sf)
library(terra)
library(tidyverse)
library(irg)

range_summer <- st_read(
  "C:/Users/jseider.stu/Sync/Data/Porcupine/PorcupineCaribouHerdRange/NOS_TS_C_180701_Caribou_PorcupineSummerRange.shp"
) %>%
  st_geometry()

ndvi_sample <- read.csv(
  "C:/Users/jseider.stu/Sync/Data/Porcupine/SummerRange_NDVI_TimeSeries_500Sample.csv"
) %>%
  filter(NDVI > 0) %>%
  extract(
    date,
    into = c("year", "doy"),
    regex = "(\\d{4})-\\d{2}-(\\d+)",
    convert = TRUE
  ) %>%
  mutate(real_date = as.Date(doy - 1, origin = paste0(year, "-01-01"))) %>%
  # 1. Extract the numbers inside the brackets [lon, lat]
  # The regex looks for content between '[' and ']'
  mutate(clean_coords = str_extract(.geo, "(?<=\\[).+?(?=\\])")) %>%
  # 2. Split that string into two separate columns at the comma
  separate(clean_coords, into = c("lon", "lat"), sep = ",") %>%
  # 3. Convert them from text ("69.13") to numbers (69.13)
  mutate(
    lon = as.numeric(lon),
    lat = as.numeric(lat)
  ) %>%
  select(-.geo)

sites <- ndvi_sample %>%
  select(point_id, lat, lon) %>%
  group_by(point_id) %>%
  summarize(lat = mean(lat), lon = mean(lon))

ndvi_simple <- ndvi_sample %>%
  group_by(point_id, doy, real_date, lat, lon) %>%
  summarise(
    mean_ndvi = mean(NDVI, na.rm = TRUE),
    sd_ndvi = sd(NDVI, na.rm = TRUE),
    n_pixels = n(), # Useful to know if a day had partial data coverage
    .groups = "drop" # Ungroups the data after summarizing
  )


ndvi_subsample <- ndvi_simple %>%
  # filter(point_id == site_id) %>%
  setDT() %>%
  mutate(
    id = point_id,
    DayOfYear = doy,
    NDVI = mean_ndvi,
    yr = as.numeric(format(as.Date(real_date), "%Y")),
    SummaryQA = 0,
    .keep = "none"
  ) %>%
  data.table::setalloccol()

filter_ndvi(ndvi_subsample)
scale_ndvi(ndvi_subsample)
scale_doy(ndvi_subsample)

model_start(ndvi_subsample, id = "id", year = "yr")

mods <- model_params(
  ndvi_subsample,
  returns = "columns",
  id = "id",
  year = "yr",
  xmidS = "xmidS_start",
  xmidA = "xmidA_start",
  scalS = 0.05,
  scalA = 0.01
)

fit <- model_ndvi(mods, observed = TRUE)

calc_irg(ndvi_subsample)

cols <- c("IRG" = "#12c62f", "NDVI" = "#47694d")

##-----##
site_id <- 126
##-----##

ggplot(fit[id == site_id], aes(x = DayOfYear)) +
  geom_line(aes(y = irg, color = 'IRG')) +
  geom_line(aes(y = fitted, color = 'NDVI')) +
  geom_point(
    aes(y = scaled),
    data = ndvi_subsample[id == site_id],
    alpha = 0.1
  ) +
  # scale_color_manual(values = cols) +
  labs(
    y = 'NDVI (Scaled)',
    x = "Day of Year",
    color = ''
  )


coastal <- c(19)
plain <- c(72)
alpine <- c(457)

latitudinal_data <- fit %>%
  filter(id %in% c(coastal, plain, alpine)) %>%
  mutate(
    cat = case_when(
      id %in% coastal ~ "coastal",
      id %in% plain ~ "plain",
      id %in% alpine ~ "alpine"
    )
  )

# 1. Find the day where Fitted NDVI is highest for each category
peak_ndvi_dates <- latitudinal_data %>%
  group_by(cat) %>%
  # Find the row with the max fitted value
  slice_max(fitted, n = 1) %>%
  ungroup()

ggplot(latitudinal_data, aes(x = DayOfYear)) +
  # Map color to the category, and group by id to draw distinct lines
  geom_line(aes(y = fitted, color = cat, group = id), linewidth = 1) +
  geom_point(aes(y = NDVI, color = cat), alpha = 0.1) +
  scale_color_manual(
    values = c(
      "coastal" = "#408bd6ff",
      "plain" = "#0f440aff",
      "alpine" = "#b43eaeff"
    ),
    breaks = c("coastal", "plain", "alpine")
  ) +
  labs(
    title = "Latitudinal Gradient: Fitted NDVI",
    y = 'NDVI (Fitted)',
    x = "Day of Year",
    color = 'Ecotype'
  ) +
  theme_bw()
