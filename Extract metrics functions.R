# ── Libraries ─────────────────────────────────────────────────────────────────

library(openxlsx) # Read Excel files (.xls / .xlsx)
library(dplyr)    # Data manipulation
library(elevatr)  # Extract elevation data
library(sf)       # Handle shapefiles and vector spatial data
library(terra)    # Handle raster spatial data

# ── Study Area ────────────────────────────────────────────────────────────────

# Define bounding box coordinates (WGS84 / EPSG:4326)
bbox_coords <- matrix(
  c(
    -79.300, 0.36,  # Bottom-left
    -78.896, 0.36,  # Bottom-right
    -78.896, 0.74,  # Top-right
    -79.300, 0.74,  # Top-left
    -79.300, 0.36   # Close polygon
  ),
  ncol = 2,
  byrow = TRUE
)

# Create study area as an sf polygon
study_area <- st_polygon(list(bbox_coords)) %>%
  st_sfc(crs = 4326) %>%
  st_sf()

# ── Prediction Grid ───────────────────────────────────────────────────────────

# Project study area to UTM Zone 18N (EPSG:32618) for metric grid creation
study_area_utm <- st_transform(study_area, crs = 32618)

# Build a 300 m × 300 m grid over the projected study area
grid_utm <- st_make_grid(
  study_area_utm,
  cellsize = c(300, 300),
  square = TRUE
) %>%
  st_sf()

# Reproject grid back to WGS84 for consistency with other datasets
grid_wgs84 <- st_transform(grid_utm, crs = 4326)

# Extract grid cell centroids as a lon/lat data frame
grid_points <- grid_wgs84 %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(longitude = X, latitude = Y)

# ── Import Data ───────────────────────────────────────────────────────────────

# Load grid from Excel
df_grid <- read.xlsx(
  "C:/Users/Dell/Desktop/MKY_Data.xlsx",
  sheet = "grid"
) %>%
  as_tibble()

# ── Elevation & Slope Functions ───────────────────────────────────----────────

# Extract elevation and slope from a local raster (European Space Agency, 2024. 
# Copernicus Global Digital Elevation Model) for each grid point

extract_elevation <- function(df, elev_tif) {
  elev_raster  <- rast(elev_tif)
  slope_raster <- terrain(elev_raster, v = "slope", unit = "degrees")
  coords       <- df %>% select(Longitude, Latitude)
  
  df %>%
    mutate(
      Elevation = terra::extract(elev_raster,  coords)[, 2],
      Slope     = terra::extract(slope_raster, coords)[, 2]
    )
}

# ── Extract Means Values Within Buffers For Any Raster  Function ──────────────

# This function is used to extract Integrity, NDVI, JRC_TMF Intact Forest Cover,
# ESA WorldCover Forest Cover.

extract_buffered_means <- function(points_sf,
                                   raster,
                                   buffer_sizes,
                                   var_name,
                                   crs_proj = "EPSG:32617") {
  
  # Reproject points and raster to metric CRS for accurate buffering
  points_proj  <- st_transform(points_sf, crs_proj)
  raster_proj  <- project(raster, crs_proj)
  raster_vect  <- vect(points_proj) # Convert once, reuse across buffers
  
  # Progress tracking
  n_buffers <- length(buffer_sizes)
  n_points  <- nrow(points_proj)
  cat(sprintf("Processing %d points across %d buffer sizes...\n", n_points, n_buffers))
  start_time <- Sys.time()
  
  # Build a named list of extracted values, then bind all at once
  buffer_results <- lapply(seq_along(buffer_sizes), function(i) {
    size         <- buffer_sizes[i]
    buffer_start <- Sys.time()
    
    # Buffer → extract mean raster values
    mean_vals <- st_buffer(points_proj, dist = size) %>%
      vect() %>%
      extract(raster_proj, ., fun = mean, na.rm = TRUE) %>%
      .[ , 2] # Drop ID column, keep values only
    
    # Progress report
    buffer_time <- as.numeric(difftime(Sys.time(), buffer_start, units = "secs"))
    elapsed     <- as.numeric(difftime(Sys.time(), start_time,  units = "mins"))
    eta         <- (elapsed / i) * (n_buffers - i)
    cat(sprintf("  [%d/%d] %dm buffer: %.1fs | Elapsed: %.1fm | ETA: %.1fm\n",
                i, n_buffers, size, buffer_time, elapsed, eta))
    
    # Return as named data frame column
    setNames(data.frame(mean_vals), paste0(var_name, "_", size, "m"))
  })
  
  # Bind all buffer columns to the original sf object
  output_df <- bind_cols(points_proj, do.call(cbind, buffer_results))
  
  total_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  cat(sprintf("Done. Total time: %.1f minutes\n", total_time))
  
  return(output_df)
}

# ── FLII (Integrity) Preparation ──────────────────────────────────────────────

# Load, crop, and clean FLII raster
map_flii <- rast("C:/Users/Dell/Desktop/flii_SouthAmerica.tif") %>%
  crop(study_area) %>%
  mask(study_area) %>%
  ifel(. < 0, 0, .) # Recode negative values (agriculture areas) to 0 (lowest forest integrity)

# Rescale FLII to a 0–10 index
rescale_0_10 <- function(x) {
  ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))) * 10
}
map_flii <- app(map_flii, rescale_0_10)

# Convert to data frame for plotting (retains x/y coordinates)
map_flii_df <- as.data.frame(map_flii, xy = TRUE) %>%
  na.omit() %>%
  rename(FLII = 3) # Rename the raster value column (column 3) to FLII

# ── JRC TMF (Intact Forest Cover) Preparation ──────────────────────────-------

# Load, crop, and binarize JRC TMF raster (1 = forest, 0 = non-forest)
map_jrc_tmf <- rast("C:/Users/Dell/Desktop/JRC_TMF.tif") %>%
  crop(study_area) %>%
  mask(study_area) %>%
  ifel(. == 1, 1, 0) # Binarize: retain forest pixels (code 1), set others to 0

# ── Distance to Water Function ──────────────────────────────────────────----──

# Extract minimum Euclidean distance to water pixels for each grid point
extract_water_distances <- function(grid_sf, raster, water_code = 80) {
  
  # Binarize raster and extract water pixel coordinates as sf points
  water_sf <- raster %>%
    ifel(. == water_code, 1, NA) %>% # Keep only water pixels
    as.points(values = TRUE) %>%
    st_as_sf()
  
  # Progress tracking setup
  n_points  <- nrow(grid_sf)
  distances <- numeric(n_points)
  start_time  <- Sys.time()
  pb_interval <- max(1, floor(n_points / 100)) # Report every ~1%
  cat(sprintf("Calculating distances for %d points...\n", n_points))
  
  for (i in seq_len(n_points)) {
    distances[i] <- min(st_distance(grid_sf[i, ], water_sf))
    
    if (i %% pb_interval == 0 || i == n_points) {
      elapsed  <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      rate     <- i / elapsed
      eta      <- (n_points - i) / rate
      cat(sprintf("\r%6.1f%% | %d/%d | %.1f pts/sec | ETA: %.0fs",
                  round(100 * i / n_points, 1), i, n_points, rate, eta))
      flush.console()
    }
  }
  
  elapsed_total <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat(sprintf("\nCompleted in %.1f seconds\n", elapsed_total))
  
  grid_sf$dist_to_water_m <- distances
  return(grid_sf)
}

# ── Distance to Forest Edge Function ──────────────────────----────────────────

# Calculate signed distance to nearest forest edge for each grid point
extract_forest_edge_distances <- function(grid_sf, raster, forest_code = 1) {
  
  # Binarize raster and extract forest / non-forest pixels as sf points
  forest_sf <- raster %>%
    ifel(. == forest_code, 1, NA) %>%
    as.points(values = TRUE) %>%
    st_as_sf()
  
  non_forest_sf <- raster %>%
    ifel(. != forest_code, 1, NA) %>%
    as.points(values = TRUE) %>%
    st_as_sf()
  
  # Classify each grid point as inside or outside forest
  in_forest <- terra::extract(raster, vect(grid_sf))[, 2] == forest_code
  
  grid_in_forest  <- grid_sf[in_forest, ]
  grid_out_forest <- grid_sf[!in_forest, ]
  
  cat(sprintf("Grid points — inside forest: %d | outside forest: %d\n",
              nrow(grid_in_forest), nrow(grid_out_forest)))
  start_time <- Sys.time()
  
  # Outside forest: positive distance to nearest forest pixel
  if (nrow(grid_out_forest) > 0) {
    cat("Calculating distances for outside-forest points...\n")
    grid_out_forest$dist_to_edge_m <- st_distance(grid_out_forest, forest_sf) %>%
      apply(1, min)
  }
  
  # Inside forest: negative distance to nearest non-forest pixel
  if (nrow(grid_in_forest) > 0) {
    cat("Calculating distances for inside-forest points...\n")
    grid_in_forest$dist_to_edge_m <- st_distance(grid_in_forest, non_forest_sf) %>%
      apply(1, min) %>%
      `*`(-1) # Negate to indicate depth inside forest
  }
  
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat(sprintf("Completed in %.1f seconds\n", elapsed))
  
  # Recombine and return
  rbind(grid_out_forest, grid_in_forest)
}

# ── Road Density Extraction ───────────────────────────────────────────────────

#' Load and crop GRIP road network to the study area
load_roads <- function(shp_path, study_area) {
  st_read(shp_path) %>%
    st_transform(4326) %>%
    st_intersection(study_area)
}

# Compute road density (m/m²) within buffers around points
extract_road_density <- function(points_sf, roads, radius, id_col = "Grid_ID") {
  buffers <- st_buffer(points_sf, dist = radius)
  col_name <- paste0("Road_density_", radius, "m")
  
  density_df <- st_intersection(roads, buffers) %>%
    mutate(length_m = as.numeric(st_length(geometry))) %>%
    st_drop_geometry() %>%
    group_by(.data[[id_col]]) %>%
    summarise(road_length_m = sum(length_m), .groups = "drop") %>%
    mutate(!!col_name := road_length_m / (pi * radius^2)) %>%
    select(all_of(id_col), all_of(col_name))
  
  # Fill points with no roads with 0
  buffers %>%
    st_drop_geometry() %>%
    select(all_of(id_col)) %>%
    left_join(density_df, by = id_col) %>%
    mutate(!!col_name := tidyr::replace_na(.data[[col_name]], 0))
}
