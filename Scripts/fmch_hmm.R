library(terra)
library(sf)
library(dplyr)
library(hmmTMB)
library(momentuHMM)
library(adehabitatLT)

# Function from Theo Michelot tutorial
split_at_gap <- function(data, max_gap = 60, shortest_track = 0) {
  # Number of tracks
  n_tracks <- length(unique(data$ID))

  # Save old ID and reinitialise ID column
  data$ID_old <- data$ID
  data$ID <- character(nrow(data))

  # Loop over tracks (i.e., over IDs)
  for (i_track in 1:n_tracks) {
    # Indices for this track
    ind_this_track <- which(data$ID_old == unique(data$ID_old)[i_track])
    track_length <- length(ind_this_track)

    # Time intervals in min
    dtimes <- difftime(
      data$time[ind_this_track[-1]],
      data$time[ind_this_track[-track_length]],
      units = "mins"
    )

    # Indices of gaps longer than max_gap
    ind_gap <- c(0, which(dtimes > max_gap), track_length)

    # Create new ID based on split track
    subtrack_ID <- rep(1:(length(ind_gap) - 1), diff(ind_gap))
    data$ID[ind_this_track] <- paste0(
      data$ID_old[ind_this_track],
      "-",
      subtrack_ID
    )
  }

  # Only keep sub-tracks longer than some duration
  track_lengths <- sapply(unique(data$ID), function(id) {
    ind <- which(data$ID == id)
    difftime(data$time[ind[length(ind)]], data$time[ind[1]], units = "min")
  })
  ID_keep <- names(track_lengths)[which(track_lengths >= shortest_track)]
  data <- subset(data, ID %in% ID_keep)

  return(data)
}

data <- read.csv(
  "C:/Users/jseider.stu/Sync/Data/FortymileCaribou/VideoCollars/dc_JANEdata_eating_modlocs.csv"
) %>%
  select(x_, y_, t_, id, behavior, julian, temp_mean, tpi, tri, elev) %>%
  rename(x = x_, y = y_, time = t_, ID = id) %>%
  mutate(time = as.POSIXct(.$time, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))

llcoord <- st_as_sf(
  data[, c("x", "y")],
  coords = c("x", "y"),
  crs = "EPSG:4326"
)
utmcoord <- st_transform(llcoord, crs = "EPSG:3338") # Alaska Albers

data[, c("x", "y")] <- st_coordinates(utmcoord) / 1000
data <- data[order(data$ID, data$time), ]

data_split <- split_at_gap(data = data, max_gap = 24 * 60, shortest_track = 0)

data_ade <- setNA(
  ltraj = as.ltraj(
    xy = data_split[, c("x", "y")],
    date = data_split$time,
    id = data_split$ID
  ),
  date.ref = data_split$time[1],
  dt = 24 * 60,
  tol = 12 * 60,
  units = "min"
)

data_na <- ld(data_ade)[, c("id", "x", "y", "date")]
colnames(data_na) <- c("ID", "x", "y", "time")

data_na <- data_na %>%
  group_by(ID) %>%
  filter(n() > 70) %>%
  ungroup() %>%
  as.data.frame()

data_hmm <- prepData(data_na, type = "UTM")

dist <- list(step = "gamma", angle = "vm")

Par0_2s <- list(step = c(0.05, 0.2, 0.05, 0.2), angle = c(0.1, 3))

hmm <- fitHMM(data_hmm, nbStates = 2, dist = dist, Par0 = Par0_2s)
hmm
