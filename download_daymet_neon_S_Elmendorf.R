# Download Daymet temperature & precipitation for all NEON sites (adjusts start >= 1980)

# Required packages
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("purrr", quietly = TRUE)) install.packages("purrr")
if (!requireNamespace("tibble", quietly = TRUE)) install.packages("tibble")
if (!requireNamespace("daymetr", quietly = TRUE)) install.packages("daymetr")

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(daymetr)

download_neon_daymet <- function(dest_dir = "c:/Users/Sarah/Documents/git/tst",
                                 start_year = 1980,
                                 end_year = as.character(as.numeric(format(Sys.Date(), "%Y"))-1),
                                 pause_seconds = 1,
                                 daily_vars = c("tmax", "tmin", "prcp")) {
  # Daymet v4 coverage begins in 1980; adjust if user requests earlier start
  actual_start <- max(as.integer(start_year), 1980)
  end_year <- as.integer(end_year)
  if (end_year < actual_start) stop("end_year must be >= start_year (after adjustment).")
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 1) Get NEON site metadata (siteCode + lat/lon)
  neon_sites_url <- "https://data.neonscience.org/api/v0/sites?limit=2000"
  res <- httr::GET(neon_sites_url)
  if (httr::status_code(res) != 200) stop("Failed to fetch NEON sites from API.")
  sites_json <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), flatten = TRUE)
  sites_df <- as_tibble(sites_json$data) |>
    select(siteCode, siteLongitude, siteLatitude) |>
    filter(!is.na(siteLongitude) & !is.na(siteLatitude)) %>%
    rename(latitude = siteLatitude, longitude = siteLongitude) #%>%
    #head()
  
  # 2) For each site, call daymetr::download_daymet; capture results
  results <- sites_df |>
    mutate(
      requested_start = as.integer(start_year),
      actual_start = actual_start,
      end_year = end_year
    ) |>
    pmap_dfr(function(siteCode, latitude, longitude, requested_start, actual_start, end_year) {
      out_row <- tibble(
        siteCode = siteCode,
        lat = latitude,
        lon = longitude,
        requested_start = requested_start,
        actual_start = actual_start,
        end_year = end_year,
        status = NA_character_,
        file = NA_character_
      )
      # Safe wrapper: download data internally, attach siteCode, write per-site CSV with unique name
      try_res <- tryCatch({
        dm_raw <- daymetr::download_daymet(
          lat = latitude,
          lon = longitude,
          start = actual_start,
          end = end_year,
          internal = TRUE,    # return data to R instead of writing generic file
          simplify = FALSE #,
          #var = daily_vars
        )
        # Normalize returned object: if list with $data use that, else if data.frame use as-is
        if (is.list(dm_raw) && !is.data.frame(dm_raw) && "data" %in% names(dm_raw)) {
          dm_df <- dm_raw$data
        } else if (is.data.frame(dm_raw)) {
          dm_df <- dm_raw
        } else {
          stop("Unexpected object returned from daymetr::download_daymet")
        }
        # Attach siteCode column
        dm_df$siteCode <- siteCode

        # Safe filename: sanitize siteCode and build unique filename
        safe_site <- gsub("[^A-Za-z0-9_-]", "_", siteCode)
        fname <- paste0(safe_site, "_daymet_", actual_start, "-", end_year, ".csv")
        out_path <- file.path(dest_dir, fname)

        # Ensure dest_dir exists
        dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

        # Write CSV (use write.csv for base R; no printing)
        utils::write.csv(dm_df, out_path, row.names = FALSE)

        out_row$status <- "ok"
        out_row$file <- out_path
        out_row
      }, error = function(e) {
        out_row$status <- paste0("error: ", conditionMessage(e))
        out_row
      })
      Sys.sleep(pause_seconds)  # gentle pacing
      try_res
    })

  # Return results invisibly (useful object)
  invisible(results)
}



# read in each file (one per site) in the created directory and identify heatwaves
# where heat wave is defined as 3 or more consecutive days with max temp > 95th percentile max temp
# for that day of year
#Other studies use relative threshold exceedance (e.g., 95p) for a predefined number of days
# # (e.g., Fischer and Schär 2010).

# Ensure required packages for downstream processing
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")

library(readr)
library(lubridate)

# Read all CSVs in dest_dir (one file per site) into a single tibble.
# Keeps siteCode if present; otherwise infers from filename.
read_daymet_dir <- function(dest_dir) {
  files <- list.files(dest_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) return(tibble::tibble())
  
  df <- purrr::map_dfr(files, function(f) {
    dat <- readr::read_csv(f, show_col_types = FALSE)
    dat$._source_file <- f
    dat
  })
  
  # Ensure siteCode column exists (use filename when absent)
  # if (!"siteCode" %in% names(df)) {
  #   df <- df |>
  #     mutate(
  #       siteCode = gsub("_.*$", "", basename(._source_file))
  #     )
  # }
  
  # Normalize date column: prefer an existing date, else build from year+yday/doy
  # df <- df |>
  #   mutate(
  #     # possible column names for day-of-year: yday, doy, dayofyear
  #     doy = coalesce(
  #       dplyr::coalesce(!!!purrr::map(c("yday", "doy", "day_of_year", "dayofyear"), ~ifelse(.x %in% names(.), .[[.x]], NA_integer_))),
  #       NA_integer_
  #     )
  #   )
  
  # Create a proper Date column
  if ("date" %in% names(df)) {
    df <- df |> mutate(date = as.Date(date))
  } else if (any(c("yday", "doy", "day_of_year", "dayofyear") %in% names(df)) && "year" %in% names(df)) {
    doy_col <- intersect(c("yday", "doy", "day_of_year", "dayofyear"), names(df))[1]
    df <- df |>
      mutate(
        doy = as.integer(.data[[doy_col]]),
        date = as.Date(paste0(year, "-01-01")) + (doy - 1)
      )
  } else {
    df <- df |> mutate(date = as.Date(NA))
  }

  # Drop helper file column after using it for inference
  df <- df |> select(-._source_file)

  df
}

# Identify heatwaves:
# - threshold_pct: percentile (e.g., 95)
# - min_days: minimum consecutive days above threshold to be a heatwave (e.g., 3)
# - ref_start/ref_end: reference period used to compute the day-of-year thresholds (default 1991-2020)
# Returns list: combined_data (with columns date, siteCode, tmax, is_heatwave_day, heatwave_id)
# and heatwaves (one row per heatwave with start/end/duration)
compute_heatwaves <- function(df, threshold_pct = 99, min_days = 5,
                              ref_start = 1991, ref_end = 2020,
                              tmax_candidates = c("tmax", "tmax_c", "tmax.C", "TMAX",
                                                  "tmax..deg.c.")) {
  # detect tmax column
  tmax_col <- names(df)[tolower(names(df)) %in% tolower(tmax_candidates)]
  if (length(tmax_col) != 1) stop("No tmax column found in data. Expected one of: ", paste(tmax_candidates, collapse = ", "))
  tmax_col <- tmax_col[1]
  
  # ensure date and siteCode
  if (!"date" %in% names(df)) stop("date column missing; ensure read_daymet_dir produced a date column.")
  if (!"siteCode" %in% names(df)) stop("siteCode column missing.")
  
  # compute doy and year, and tmax
  df2 <- df |>
    mutate(
      date = as.Date(date),
      doy = lubridate::yday(date),
      year = lubridate::year(date),
      tmax = .data[[tmax_col]]
    ) |>
    filter(!is.na(date) & !is.na(tmax))
  
  # Compute thresholds using the reference period (ref_start:ref_end).
  # If no data exists in the reference period for a site/doy, fall back to all available years for that site/doy.
  df_ref <- df2 |>
    filter(year >= as.integer(ref_start) & year <= as.integer(ref_end))
  
  # if (nrow(df_ref) == 0) {
  #   warning("No data found in reference period ", ref_start, "-", ref_end, ". Using all years to compute thresholds.")
  #   df_ref <- df2
  # }
  
  # thresholds computed from reference dataset
  thresholds <- df_ref |>
    group_by(siteCode, doy) |>
    summarize(
      threshold = quantile(tmax, probs = threshold_pct/100, na.rm = TRUE),
      n_ref = sum(!is.na(tmax)),
      .groups = "drop"
    )
  
  
  # join thresholds and flag hot days
  df_flagged <- df2 |>
    left_join(thresholds, by = c("siteCode", "doy")) |>
    mutate(is_hot = ifelse(!is.na(threshold) & tmax > threshold, TRUE, FALSE))
  
  # identify contiguous runs of is_hot within each site
  df_runs <- df_flagged |>
    arrange(siteCode, date) |>
    group_by(siteCode) |>
    mutate(
      run_id = {
        r <- rle(is_hot)
        rep(seq_along(r$lengths), r$lengths)
      }
    ) |>
    group_by(siteCode, run_id) |>
    mutate(run_len = n(), run_is_hot = unique(is_hot)) |>
    ungroup()
  
  # mark heatwave days (runs that are hot and meet min_days)
  df_runs <- df_runs |>
    mutate(
      is_heatwave_day = ifelse(run_is_hot & run_len >= min_days, TRUE, FALSE),
      heatwave_id = ifelse(is_heatwave_day, paste0(siteCode, "_hw_", run_id), NA_character_)
    )
  
  # summarize heatwaves (one row per heatwave run)
  heatwaves <- df_runs |>
    filter(is_heatwave_day) |>
    group_by(siteCode, heatwave_id) |>
    summarize(
      start_date = min(date),
      end_date = max(date),
      duration_days = as.integer(difftime(max(date), min(date), units = "days")) + 1,
      peak_tmax = max(tmax, na.rm = TRUE),
      .groups = "drop"
    )
  
  list(
    combined = df_runs,
    heatwaves = heatwaves,
    thresholds_used = thresholds  # include thresholds for inspection if needed
  )
}

# Example usage (no printing):
# all_daily <- read_daymet_dir("c:/Users/Sarah/Documents/git/tst")
# hw_results <- compute_heatwaves(all_daily, threshold_pct = 95, min_days = 3)
# hw_results$heatwaves  # useful object returned for inspection

# Ensure ggplot2 is available for plotting
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)

# Create timeseries plots of heatwaves: one plot per site (x = year, y = doy), colored points for heatwave_id.
# hw_results_or_df: either a list returned by compute_heatwaves() or the combined dataframe with is_heatwave_day & heatwave_id
# out_dir: directory to save PNGs when save = TRUE
# Returns a named list of ggplot objects (invisibly).
plot_heatwaves_timeseries <- function(hw_results_or_df, out_dir = file.path(getwd(), "plots"), save = TRUE,
                    width = 8, height = 4.5, dpi = 300, point_size = 1.6,
                    xrange = c('1980-01-01','2024-01-01')
) {
  # helper: convert xrange to numeric years
  parse_xrange_to_years <- function(xr) {
    if (length(xr) != 2) stop("xrange must be length 2 (start, end)")
    if (inherits(xr, "Date")) {
      yr <- lubridate::year(xr)
      return(as.numeric(yr))
    }
    if (is.character(xr)) {
      parsed <- tryCatch(as.Date(xr), error = function(e) NA)
      if (!any(is.na(parsed))) {
        return(as.numeric(lubridate::year(parsed)))
      }
      numeric_try <- suppressWarnings(as.numeric(xr))
      if (!any(is.na(numeric_try))) return(numeric_try)
      stop("xrange character values must be parseable as Dates or numeric years")
    }
    if (is.numeric(xr)) return(as.numeric(xr))
    stop("Unsupported type for xrange")
  }

  x_limits <- parse_xrange_to_years(xrange)
  if (x_limits[1] > x_limits[2]) stop("xrange start must be <= end")

  # obtain combined dataframe
  combined <- if (is.list(hw_results_or_df) && "combined" %in% names(hw_results_or_df)) {
    hw_results_or_df$combined
  } else {
    hw_results_or_df
  }
  
  # basic checks
  if (!"siteCode" %in% names(combined)) stop("siteCode column missing in input data.")
  if (!"date" %in% names(combined)) stop("date column missing in input data.")
  if (!"doy" %in% names(combined)) combined <- combined |> mutate(doy = lubridate::yday(date))
  if (!"is_heatwave_day" %in% names(combined)) combined <- combined |> mutate(is_heatwave_day = FALSE)
  if (!"heatwave_id" %in% names(combined)) combined <- combined |> mutate(heatwave_id = NA_character_)

  # filter to heatwave days only
  hw_points <- combined |> filter(is_heatwave_day & !is.na(date))
  sites <- unique(hw_points$siteCode)
  if (length(sites) == 0) return(invisible(list()))

  if (save) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  plots <- vector("list", length(sites))
  names(plots) <- sites

  for (site in sites) {
    dsub <- hw_points |> filter(siteCode == site) |> mutate(year = lubridate::year(date))
    if (nrow(dsub) == 0) {
      plots[[site]] <- NULL
      next
    }
    
    hw_counts <- dsub |>
      filter(!is.na(heatwave_id)) |>
      distinct(heatwave_id, year) |>
      count(year, name = "n")

    p <- ggplot(dsub, aes(x = year, y = doy, color = tmax..deg.c.)) +
      geom_jitter(height = 0.3, width = 0.2, size = point_size, alpha = 0.85) +
      scale_color_gradientn(
        colors = c("#fff5f0","#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#99000d"),
        name = "tmax (°C)",
        limits = NULL
      ) +
      scale_x_continuous(
        limits = c(x_limits[1]-1, x_limits[2]+1)#, 
        #breaks = c(floor(x_limits[1]), ceiling(x_limits[2]))
      ) +
      scale_y_continuous(limits = c(0, 366)) +
      labs(
        title = paste0("Heatwave days — ", site),
        x = "Year",
        y = "Day of year"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom", plot.title = element_text(size = 12, face = "bold"))

    if (nrow(hw_counts) > 0) {
      p <- p +
        geom_point(
          data = hw_counts,
          mapping = aes(x = year, y = 0.5, size = n),
          inherit.aes = FALSE,
          color = "black",
          alpha = 0.8
        ) +
        scale_size_continuous(name = "Heatwaves / year", range = c(1.5, 6))
    }

    plots[[site]] <- p
    if (save) {
      safe_site <- gsub("[^A-Za-z0-9_-]", "_", site)
      fname <- file.path(out_dir, paste0(safe_site, "_heatwave_timeseries.png"))
      ggplot2::ggsave(filename = fname, plot = p, width = width, height = height, dpi = dpi)
    }
  }
  invisible(plots)
}


# Example usage:
# downloads <- download_neon_daymet(start_year = 1970, end_year = 2024)

# all_daily <- read_daymet_dir("c:/Users/Sarah/Documents/git/tst")
# hw_results <- compute_heatwaves(all_daily, threshold_pct = 99, min_days = 5, ref_start = 1991, ref_end = 2020)
# plots <- plot_heatwaves_timeseries(hw_results, out_dir = "c:/Users/Sarah/Documents/git/tst/plots", save = TRUE)
# names(plots)