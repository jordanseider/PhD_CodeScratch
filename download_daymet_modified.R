###-------------------------------------------------------------------###
### Function to download Daymet data at point locations 
### This function has been modified from code shared by Sarah Elmendorf
### Modified by Jordan Seider to allow any set of lat/long, use with caution
###-------------------------------------------------------------------###

test_set <- data.frame(
  siteCode = c("Site_A", "Site_B", "Site_C", "Site_D", "Site_E"),
  latitude = c(69.57554, 69.57569, 69.57762, 69.57496, 69.57651),
  longitude = c(-138.9044, -138.9053, -138.9127, -138.8949, -138.8670)
)

download_daymet <- function(
  dest_dir, # Destination directory
  start_year = 1980,
  end_year = as.character(as.numeric(format(Sys.Date(), "%Y")) - 1), # Default value is current year minus 1 (previous complete year)
  pause_seconds = 1,
  daily_vars = c("tmax", "tmin", "prcp"),
  use_neon = FALSE, # Original code by Sarah Elmendorf used sites from NEON project, set to FALSE to use different site locations
  sites_df = NULL
) {
  # `sites_df` dataframe formatted with columns: 'siteCode', 'longitude', 'latitude'

  # Daymet v4 coverage begins in 1980; adjust if user requests earlier start
  actual_start <- max(as.integer(start_year), 1980)
  end_year <- as.integer(end_year)

  if (end_year < actual_start) {
    stop("end_year must be >= start_year (after adjustment).")
  }
  if (use_neon == FALSE & is.null(sites_df)) {
    stop("Must specify `sites_df` if use_neon is FALSE.")
  }

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  # Get NEON site metadata (siteCode + lat/lon)
  if (use_neon == TRUE) {
    neon_sites_url <- "https://data.neonscience.org/api/v0/sites?limit=2000"
    res <- httr::GET(neon_sites_url)
    if (httr::status_code(res) != 200) {
      stop("Failed to fetch NEON sites.")
    }
    sites_json <- jsonlite::fromJSON(
      httr::content(res, as = "text", encoding = "UTF-8"),
      flatten = TRUE
    )
    sites_df <- tibble::as_tibble(sites_json$data) |>
      dplyr::select(siteCode, siteLongitude, siteLatitude) |>
      dplyr::filter(!is.na(siteLongitude) & !is.na(siteLatitude)) |>
      dplyr::rename(latitude = siteLatitude, longitude = siteLongitude)
  }

  # Ensure sites_df dataframe is correctly formatted
  required_cols <- c("siteCode", "latitude", "longitude")
  missing_cols <- setdiff(required_cols, names(sites_df))

  if (length(missing_cols) > 0) {
    stop(paste0(
      "Incorrect sites_df formatting. Missing column(s) or incorrect spelling: ",
      paste(missing_cols, collapse = ", "),
      ". \nRequired headers are: 'siteCode', 'latitude', and 'longitude'."
    ))
  }

  # For each site, call daymetr::download_daymet; capture results
  results <- sites_df |>
    dplyr::mutate(
      requested_start = as.integer(start_year),
      actual_start = actual_start,
      end_year = end_year
    ) |>
    purrr::pmap_dfr(function(
      siteCode,
      latitude,
      longitude,
      requested_start,
      actual_start,
      end_year
    ) {
      out_row <- tibble::tibble(
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
      try_res <- tryCatch(
        {
          # Safe filename: sanitize siteCode and build unique filename
          safe_site <- gsub("[^A-Za-z0-9_-]", "_", siteCode)
          fname <- paste0(
            safe_site,
            "_daymet_",
            actual_start,
            "-",
            end_year,
            ".csv"
          )
          out_path <- file.path(dest_dir, fname)

          # Skip iteration if output file already exists
          if (file.exists(out_path)) {
            message("Skipping site '", siteCode, "': File already exists.")
            out_row$status <- "skipped (exists)"
            out_row$file <- out_path
            return(out_row)
          }

          message("Downloading data for site '", siteCode, "'...")

          dm_raw <- daymetr::download_daymet(
            lat = latitude,
            lon = longitude,
            start = actual_start,
            end = end_year,
            internal = TRUE, # return data to R instead of writing generic file
            simplify = FALSE #,
            #var = daily_vars
          )

          # Normalize returned object: if list with $data use that, else if data.frame use as-is
          if (
            is.list(dm_raw) &&
              !is.data.frame(dm_raw) &&
              "data" %in% names(dm_raw)
          ) {
            dm_df <- dm_raw$data
          } else if (is.data.frame(dm_raw)) {
            dm_df <- dm_raw
          } else {
            stop("Unexpected object returned from daymetr::download_daymet")
          }

          # Attach siteCode column
          dm_df$siteCode <- siteCode

          # Write CSV (use write.csv for base R; no printing)
          utils::write.csv(dm_df, out_path, row.names = FALSE)

          out_row$status <- "ok"
          out_row$file <- out_path
          out_row
        },
        error = function(e) {
          out_row$status <- paste0("error: ", conditionMessage(e))
          message("Error at site '", siteCode, "': ", conditionMessage(e))
          out_row
        }
      )

      if (try_res$status == "ok") {
        Sys.sleep(pause_seconds)
      }

      try_res
    })

  # Return results invisibly (useful object)
  invisible(results)
}

download_daymet(dest_dir = "C:/Users/jseider.stu/Desktop", sites_df = test_set)
