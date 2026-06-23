# ════════════════════════════════════════════════════════════════════════════
# SPATIAL DATA EXTRACTION PIPELINE
# ════════════════════════════════════════════════════════════════════════════
#
# Project: Acoustic monitoring and habitat suitability of endangered primates
#          in the Ecuadorian Chocó (brown-headed spider monkey and howler monkey)
#
# Objective: Extract environmental variables at multiple buffer scales (100-2000m)
#           for both study plots and a regular prediction grid
#
# Data sources:
#   - Elevation: Copernicus Global DEM
#   - Canopy height: Global Canopy Height Map (CHM v2)
#   - Forest integrity: Forest Landscape Integrity Index (FLII)
#   - Vegetation: NDVI (Google Earth Engine, 2024)
#   - Forest classification: JRC Tropical Moist Forest (TMF) dataset
#   - Road network: GRIP global roads database
#
# Output: Spatial datasets with environmental predictors for GAM modeling
#
# ════════════════════════════════════════════════════════════════════════════

# ── Libraries ──────────────────────────────────────────────────────────────────

library(terra)         # Raster spatial data
library(sf)            # Vector spatial data
library(dplyr)         # Data manipulation
library(openxlsx)      # Read/write Excel files
library(future.apply)  # Parallel processing

# ════════════════════════════════════════════════════════════════════════════
# PART 1: DATA SETUP AND SPATIAL FRAMEWORK
# ════════════════════════════════════════════════════════════════════════════

cat("\n========== LOADING INPUT DATA ==========\n\n")

# Load study plots from Excel
df_plots <- read.xlsx(
  "D:/au813514/MKY_Reanalyses/MKY_Data.xlsx",
  sheet = "plots_description"
) %>%
  as_tibble()

sf_plots <- st_as_sf(
  df_plots,
  coords = c("Longitude", "Latitude"),
  crs = 4326,
  remove = FALSE
)

cat(sprintf("✓ Loaded %d study plots\n", nrow(df_plots)))

# Define study area (Canandé region, Ecuadorian Chocó)
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

study_area <- st_polygon(list(bbox_coords)) %>%
  st_sfc(crs = 4326) %>%
  st_sf()

# Create 100 m × 100 m prediction grid
create_prediction_grid <- function(study_area, cellsize, crs_proj = "EPSG:32617") {
  
  study_area_utm <- st_transform(study_area, crs = crs_proj)
  
  grid_utm <- st_make_grid(
    study_area_utm,
    cellsize = c(cellsize, cellsize),
    square = TRUE,
    what = "centers"
  ) %>%
    st_sf(geometry = .)
  
  grid_filtered <- grid_utm %>%
    st_intersection(study_area_utm)
  
  grid_wgs84 <- grid_filtered %>%
    st_transform(crs = 4326)
  
  grid_coords <- st_coordinates(grid_wgs84) %>%
    as.data.frame() %>%
    rename(Longitude = X, Latitude = Y) %>%
    mutate(Grid_ID = row_number()) %>%
    select(Grid_ID, Longitude, Latitude) %>%
    as_tibble()
  
  grid_sf <- st_as_sf(
    grid_coords,
    coords = c("Longitude", "Latitude"),
    crs = 4326,
    remove = FALSE
  )
  
  cat(sprintf("✓ Created %d grid points at %d m × %d m resolution\n",
              nrow(grid_coords), cellsize, cellsize))
  
  list(df = grid_coords, sf = grid_sf)
}

grid_output <- create_prediction_grid(study_area, cellsize = 100)
df_grid <- grid_output$df
sf_grid <- grid_output$sf

cat("\n")

# ════════════════════════════════════════════════════════════════════════════
# PART 2: EXTRACT ELEVATION AND SLOPE (Point extraction, no buffering)
# ════════════════════════════════════════════════════════════════════════════

cat("\n========== ELEVATION AND SLOPE ==========\n\n")

extract_elevation_slope <- function(points_sf, elevation) {
  
  slope_raster <- terrain(elevation, v = "slope", unit = "degrees")
  points_vect <- vect(points_sf)
  
  elev_vals <- extract(elevation, points_vect)[, 2]
  slope_vals <- extract(slope_raster, points_vect)[, 2]
  
  points_sf %>%
    mutate(
      Elevation = elev_vals,
      Slope = slope_vals
    )
}

elevation <- rast("D:/au813514/MKY_Reanalyses/Elevation.tif")

sf_plots <- extract_elevation_slope(sf_plots, elevation)
sf_grid <- extract_elevation_slope(sf_grid, elevation)

cat("✓ Elevation and slope extracted\n\n")

# ════════════════════════════════════════════════════════════════════════════
# PART 3: BUFFERED METRIC EXTRACTION (Helper functions and setup)
# ════════════════════════════════════════════════════════════════════════════

# Buffer sizes for multi-scale analysis
buffer_sizes <- c(100, 200, 500, 1000, 2000)

# Create backups directory
backup_dir <- "D:/au813514/MKY_Reanalyses/Backups/"
if (!dir.exists(backup_dir)) dir.create(backup_dir, recursive = TRUE)

# Helper: Split points into contiguous chunks for parallel processing
make_point_chunks <- function(points_proj, n_cores) {
  n <- nrow(points_proj)
  idx <- split(seq_len(n), cut(seq_len(n), n_cores, labels = FALSE))
  lapply(idx, function(i) points_proj[i, ])
}

# ── Function 1: Extract mean values from continuous rasters ──────────────────

extract_buffered_means <- function(points_sf, raster_path, buffer_sizes, 
                                   var_name, crs_proj = "EPSG:32617", n_cores = 10) {
  
  points_proj <- st_transform(points_sf, crs_proj)
  n_points <- nrow(points_proj)
  n_buffers <- length(buffer_sizes)
  
  cat(sprintf("Extracting %s: %d points x %d buffer(s) using %d cores\n",
              var_name, n_points, n_buffers, n_cores))
  start_time <- Sys.time()
  
  # Reproject raster once, write to temp file
  tmp <- tempfile(fileext = ".tif")
  terra::writeRaster(terra::project(terra::rast(raster_path), crs_proj),
                     tmp, overwrite = TRUE)
  on.exit(file.remove(tmp), add = TRUE)
  
  point_chunks <- make_point_chunks(points_proj, n_cores)
  
  plan(multisession, workers = n_cores)
  on.exit(plan(sequential), add = TRUE)
  
  result_cols <- lapply(seq_along(buffer_sizes), function(i) {
    size <- buffer_sizes[i]
    buffer_start <- Sys.time()
    
    vals_list <- future_lapply(point_chunks, function(pts_chunk) {
      r <- terra::rast(tmp)
      r <- terra::toMemory(r)
      bufs <- terra::vect(sf::st_buffer(pts_chunk, dist = size))
      terra::extract(r, bufs, fun = mean, na.rm = TRUE)[, 2]
    }, future.seed = TRUE)
    
    mean_vals <- unlist(vals_list, use.names = FALSE)
    
    buffer_time <- as.numeric(difftime(Sys.time(), buffer_start, units = "secs"))
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    eta <- (elapsed / i) * (n_buffers - i)
    cat(sprintf("  [%d/%d] %d m: %.1f sec | Elapsed: %.1f min | ETA: %.1f min\n",
                i, n_buffers, size, buffer_time, elapsed, eta))
    
    setNames(data.frame(mean_vals), paste0(var_name, "_", size, "m"))
  })
  
  output_sf <- st_as_sf(bind_cols(points_proj, do.call(cbind, result_cols)))
  cat(sprintf("✓ Done. Total time: %.1f minutes\n\n",
              as.numeric(difftime(Sys.time(), start_time, units = "mins"))))
  return(output_sf)
}

# ── Function 2: Extract category cover from categorical rasters ─────────────

extract_category_cover <- function(points_sf, raster_path, category, buffer_sizes, 
                                   var_name, crs_proj = "EPSG:32617", n_cores = 30) {
  
  points_proj <- st_transform(points_sf, crs_proj)
  n_points <- nrow(points_proj)
  n_buffers <- length(buffer_sizes)
  
  cat(sprintf("Extracting %s (category %d): %d points x %d buffer(s) using %d cores\n",
              var_name, category, n_points, n_buffers, n_cores))
  start_time <- Sys.time()
  
  # Reproject with method="near" for categorical data, binarize, write temp
  r_proj <- terra::project(terra::rast(raster_path), crs_proj, method = "near")
  r_bin <- r_proj == category
  tmp <- tempfile(fileext = ".tif")
  terra::writeRaster(r_bin, tmp, overwrite = TRUE)
  on.exit(file.remove(tmp), add = TRUE)
  rm(r_proj, r_bin); gc()
  
  point_chunks <- make_point_chunks(points_proj, n_cores)
  
  plan(multisession, workers = n_cores)
  on.exit(plan(sequential), add = TRUE)
  
  result_cols <- lapply(seq_along(buffer_sizes), function(i) {
    size <- buffer_sizes[i]
    buffer_start <- Sys.time()
    
    vals_list <- future_lapply(point_chunks, function(pts_chunk) {
      r <- terra::rast(tmp)
      r <- terra::toMemory(r)
      bufs <- terra::vect(sf::st_buffer(pts_chunk, dist = size))
      round(terra::extract(r, bufs, fun = mean, na.rm = TRUE)[, 2] * 100, 4)
    }, future.seed = TRUE)
    
    cover_vals <- unlist(vals_list, use.names = FALSE)
    
    buffer_time <- as.numeric(difftime(Sys.time(), buffer_start, units = "secs"))
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    eta <- (elapsed / i) * (n_buffers - i)
    cat(sprintf("  [%d/%d] %d m: %.1f sec | Elapsed: %.1f min | ETA: %.1f min\n",
                i, n_buffers, size, buffer_time, elapsed, eta))
    
    setNames(data.frame(cover_vals), paste0(var_name, "_", size, "m"))
  })
  
  output_sf <- st_as_sf(bind_cols(points_proj, do.call(cbind, result_cols)))
  cat(sprintf("✓ Done. Total time: %.1f minutes\n\n",
              as.numeric(difftime(Sys.time(), start_time, units = "mins"))))
  return(output_sf)
}

# ── Function 3: Extract edge density from categorical rasters ──────────────

extract_edge_density <- function(points_sf, raster_path, categories, buffer_sizes, 
                                 var_name, crs_proj = "EPSG:32617", n_cores = 30) {
  
  points_proj <- st_transform(points_sf, crs_proj)
  n_points <- nrow(points_proj)
  n_buffers <- length(buffer_sizes)
  
  cat(sprintf("Extracting %s (categories %s): %d points x %d buffer(s) using %d cores\n",
              var_name, paste(categories, collapse = "+"), n_points, n_buffers, n_cores))
  start_time <- Sys.time()
  
  # Reproject, binarize, compute edge raster
  r_proj <- terra::project(terra::rast(raster_path), crs_proj, method = "near")
  r_bin <- r_proj %in% categories
  
  w <- matrix(1, 3, 3); w[2, 2] <- 0
  neighbor_sum <- terra::focal(r_bin, w, fun = "sum", na.rm = TRUE)
  edge_raster <- r_bin * (neighbor_sum < 8)
  
  pixel_size <- terra::res(r_proj)[1]
  tmp <- tempfile(fileext = ".tif")
  terra::writeRaster(edge_raster, tmp, overwrite = TRUE)
  on.exit(file.remove(tmp), add = TRUE)
  rm(r_proj, r_bin, neighbor_sum, edge_raster); gc()
  
  point_chunks <- make_point_chunks(points_proj, n_cores)
  
  plan(multisession, workers = n_cores)
  on.exit(plan(sequential), add = TRUE)
  
  result_cols <- lapply(seq_along(buffer_sizes), function(i) {
    size <- buffer_sizes[i]
    buffer_start <- Sys.time()
    
    vals_list <- future_lapply(point_chunks, function(pts_chunk) {
      r <- terra::rast(tmp)
      r <- terra::toMemory(r)
      buf_sf <- sf::st_buffer(pts_chunk, dist = size)
      edge_sum <- terra::extract(r, terra::vect(buf_sf), fun = sum, na.rm = TRUE)[, 2]
      buf_areas <- as.numeric(sf::st_area(buf_sf))
      round((edge_sum * px_size) / buf_areas, 4)
    }, future.seed = TRUE, future.globals = list(px_size = pixel_size, size = size, tmp = tmp))
    
    edge_densities <- unlist(vals_list, use.names = FALSE)
    
    buffer_time <- as.numeric(difftime(Sys.time(), buffer_start, units = "secs"))
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    eta <- (elapsed / i) * (n_buffers - i)
    cat(sprintf("  [%d/%d] %d m: %.1f sec | Elapsed: %.1f min | ETA: %.1f min\n",
                i, n_buffers, size, buffer_time, elapsed, eta))
    
    setNames(data.frame(edge_densities), paste0(var_name, "_", size, "m"))
  })
  
  output_sf <- st_as_sf(bind_cols(points_proj, do.call(cbind, result_cols)))
  cat(sprintf("✓ Done. Total time: %.1f minutes\n\n",
              as.numeric(difftime(Sys.time(), start_time, units = "mins"))))
  return(output_sf)
}

# ── Function 4: Extract road density from line shapefile ────────────────────

extract_road_density <- function(points_sf, roads, buffer_sizes, n_cores = 30) {
  
  points_proj <- st_transform(points_sf, st_crs(roads))
  n_points <- nrow(points_proj)
  n_buffers <- length(buffer_sizes)
  
  cat(sprintf("Extracting road density: %d points x %d buffer(s) using %d cores\n",
              n_points, n_buffers, n_cores))
  start_time <- Sys.time()
  
  point_chunks <- make_point_chunks(points_proj, n_cores)
  
  plan(multisession, workers = n_cores)
  on.exit(plan(sequential), add = TRUE)
  
  result_cols <- lapply(seq_along(buffer_sizes), function(i) {
    size <- buffer_sizes[i]
    buffer_start <- Sys.time()
    
    vals_list <- future_lapply(point_chunks, function(pts_chunk) {
      bufs <- sf::st_buffer(pts_chunk, dist = size)
      bufs$buffer_id <- seq_len(nrow(bufs))
      
      inter <- suppressWarnings(sf::st_intersection(roads, bufs))
      if (nrow(inter) == 0) {
        lengths <- rep(0, nrow(bufs))
      } else {
        lt <- inter
        lt$length_m <- as.numeric(sf::st_length(lt))
        lt <- sf::st_drop_geometry(lt)
        agg <- tapply(lt$length_m, lt$buffer_id, sum, na.rm = TRUE)
        lengths <- rep(0, nrow(bufs))
        lengths[as.integer(names(agg))] <- agg
      }
      round(lengths / as.numeric(sf::st_area(bufs)), 4)
    }, future.seed = TRUE)
    
    road_densities <- unlist(vals_list, use.names = FALSE)
    
    buffer_time <- as.numeric(difftime(Sys.time(), buffer_start, units = "secs"))
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    eta <- (elapsed / i) * (n_buffers - i)
    cat(sprintf("  [%d/%d] %d m: %.1f sec | Elapsed: %.1f min | ETA: %.1f min\n",
                i, n_buffers, size, buffer_time, elapsed, eta))
    
    setNames(data.frame(road_densities), paste0("Road_density_", size, "m"))
  })
  
  output_sf <- bind_cols(points_proj, do.call(cbind, result_cols))
  cat(sprintf("✓ Done. Total time: %.1f minutes\n\n",
              as.numeric(difftime(Sys.time(), start_time, units = "mins"))))
  return(output_sf)
}

# ════════════════════════════════════════════════════════════════════════════
# PART 4: EXECUTE EXTRACTION PIPELINE
# ════════════════════════════════════════════════════════════════════════════

# Load rasters and roads
roads <- st_read("D:/au813514/MKY_Reanalyses/GRIP4_region2_cropped.shp")
jrc_path <- "D:/au813514/MKY_Reanalyses/JRC_TMF_TransitionMap_MainClasses_v1_1982_2025_SAM_ID47_N10_W80_cropped.tif"

# ── CANOPY HEIGHT (NEW SOURCE: Global Canopy Height Map v2) ─────────────────

cat("\n========== CANOPY HEIGHT (CHM v2) ==========\n")
sf_plots <- extract_buffered_means(
  sf_plots,
  raster_path = "D:/au813514/MKY_Reanalyses/CHMv2_Ecuador.tif",
  buffer_sizes = buffer_sizes,
  var_name = "Canopy_height",
  crs_proj = "EPSG:32617",
  n_cores = 10
)

sf_grid <- extract_buffered_means(
  sf_grid,
  raster_path = "D:/au813514/MKY_Reanalyses/CHMv2_Ecuador.tif",
  buffer_sizes = buffer_sizes,
  var_name = "Canopy_height",
  crs_proj = "EPSG:32617",
  n_cores = 30
)

write.csv(st_drop_geometry(sf_plots), paste0(backup_dir, "Plots_01_Canopy_height.csv"), row.names = FALSE)
write.csv(st_drop_geometry(sf_grid), paste0(backup_dir, "Grid_01_Canopy_height.csv"), row.names = FALSE)
cat("✓ Backup saved\n\n")

# ── FOREST INTEGRITY (FLII) ───────────────────────────────────────────────────

cat("\n========== FOREST INTEGRITY (FLII) ==========\n")
sf_plots <- extract_buffered_means(
  sf_plots,
  raster_path = "D:/au813514/MKY_Reanalyses/flii_SouthAmerica_cropped.tif",
  buffer_sizes = buffer_sizes,
  var_name = "Integrity",
  crs_proj = "EPSG:32617",
  n_cores = 10
)

sf_grid <- extract_buffered_means(
  sf_grid,
  raster_path = "D:/au813514/MKY_Reanalyses/flii_SouthAmerica_cropped.tif",
  buffer_sizes = buffer_sizes,
  var_name = "Integrity",
  crs_proj = "EPSG:32617",
  n_cores = 30
)

write.csv(st_drop_geometry(sf_plots), paste0(backup_dir, "Plots_02_Integrity.csv"), row.names = FALSE)
write.csv(st_drop_geometry(sf_grid), paste0(backup_dir, "Grid_02_Integrity.csv"), row.names = FALSE)
cat("✓ Backup saved\n\n")

# ── NDVI (VEGETATION PRODUCTIVITY) ────────────────────────────────────────────

cat("\n========== NDVI ==========\n")
sf_plots <- extract_buffered_means(
  sf_plots,
  raster_path = "D:/au813514/MKY_Reanalyses/NDVI_Ecuador_2024_cropped.tif",
  buffer_sizes = buffer_sizes,
  var_name = "NDVI",
  crs_proj = "EPSG:32617",
  n_cores = 10
)

sf_grid <- extract_buffered_means(
  sf_grid,
  raster_path = "D:/au813514/MKY_Reanalyses/NDVI_Ecuador_2024_cropped.tif",
  buffer_sizes = buffer_sizes,
  var_name = "NDVI",
  crs_proj = "EPSG:32617",
  n_cores = 30
)

write.csv(st_drop_geometry(sf_plots), paste0(backup_dir, "Plots_03_NDVI.csv"), row.names = FALSE)
write.csv(st_drop_geometry(sf_grid), paste0(backup_dir, "Grid_03_NDVI.csv"), row.names = FALSE)
cat("✓ Backup saved\n\n")

# ── JRC TMF FOREST CATEGORIES ──────────────────────────────────────────────────

# Category 10: Undisturbed forest
cat("\n========== JRC TMF: CATEGORY 10 (Undisturbed Forest) ==========\n")
sf_plots <- extract_category_cover(sf_plots, jrc_path, category = 10, buffer_sizes, 
                                   var_name = "Forest_undisturbed", crs_proj = "EPSG:32617")
sf_grid <- extract_category_cover(sf_grid, jrc_path, category = 10, buffer_sizes, 
                                  var_name = "Forest_undisturbed", crs_proj = "EPSG:32617")
write.csv(st_drop_geometry(sf_plots), paste0(backup_dir, "Plots_04_Forest_undisturbed.csv"), row.names = FALSE)
write.csv(st_drop_geometry(sf_grid), paste0(backup_dir, "Grid_04_Forest_undisturbed.csv"), row.names = FALSE)
cat("✓ Backup saved\n\n")

# Category 20: Degraded forest
cat("\n========== JRC TMF: CATEGORY 20 (Degraded Forest) ==========\n")
sf_plots <- extract_category_cover(sf_plots, jrc_path, category = 20, buffer_sizes, 
                                   var_name = "Forest_degraded", crs_proj = "EPSG:32617")
sf_grid <- extract_category_cover(sf_grid, jrc_path, category = 20, buffer_sizes, 
                                  var_name = "Forest_degraded", crs_proj = "EPSG:32617")
write.csv(st_drop_geometry(sf_plots), paste0(backup_dir, "Plots_05_Forest_degraded.csv"), row.names = FALSE)
write.csv(st_drop_geometry(sf_grid), paste0(backup_dir, "Grid_05_Forest_degraded.csv"), row.names = FALSE)
cat("✓ Backup saved\n\n")

# Category 30: Regrowth/Non-forest
cat("\n========== JRC TMF: CATEGORY 30 (Regrowth / Non-forest) ==========\n")
sf_plots <- extract_category_cover(sf_plots, jrc_path, category = 30, buffer_sizes, 
                                   var_name = "Forest_regrowth", crs_proj = "EPSG:32617")
sf_grid <- extract_category_cover(sf_grid, jrc_path, category = 30, buffer_sizes, 
                                  var_name = "Forest_regrowth", crs_proj = "EPSG:32617")
write.csv(st_drop_geometry(sf_plots), paste0(backup_dir, "Plots_06_Forest_regrowth.csv"), row.names = FALSE)
write.csv(st_drop_geometry(sf_grid), paste0(backup_dir, "Grid_06_Forest_regrowth.csv"), row.names = FALSE)
cat("✓ Backup saved\n\n")

# ── ROAD DENSITY ───────────────────────────────────────────────────────────────

cat("\n========== ROAD DENSITY ==========\n")
sf_plots <- extract_road_density(sf_plots, roads, buffer_sizes = buffer_sizes)
sf_grid <- extract_road_density(sf_grid, roads, buffer_sizes = buffer_sizes)
write.csv(st_drop_geometry(sf_plots), paste0(backup_dir, "Plots_07_Road_density.csv"), row.names = FALSE)
write.csv(st_drop_geometry(sf_grid), paste0(backup_dir, "Grid_07_Road_density.csv"), row.names = FALSE)
cat("✓ Backup saved\n\n")

# ── EDGE DENSITY (Undisturbed forest only) ──────────────────────────────────

cat("\n========== EDGE DENSITY: Undisturbed forest (Category 10) ==========\n")
sf_plots <- extract_edge_density(sf_plots, jrc_path, categories = 10, buffer_sizes, 
                                 var_name = "Edge_undisturbed", crs_proj = "EPSG:32617")
sf_grid <- extract_edge_density(sf_grid, jrc_path, categories = 10, buffer_sizes, 
                                var_name = "Edge_undisturbed", crs_proj = "EPSG:32617")
write.csv(st_drop_geometry(sf_plots), paste0(backup_dir, "Plots_08_Edge_undisturbed.csv"), row.names = FALSE)
write.csv(st_drop_geometry(sf_grid), paste0(backup_dir, "Grid_08_Edge_undisturbed.csv"), row.names = FALSE)
cat("✓ Backup saved\n\n")

# ── EDGE DENSITY (Undisturbed + Degraded forest) ────────────────────────────

cat("\n========== EDGE DENSITY: Undisturbed + Degraded (Categories 10+20) ==========\n")
sf_plots <- extract_edge_density(sf_plots, jrc_path, categories = c(10, 20), buffer_sizes, 
                                 var_name = "Edge_undisturbed_degraded", crs_proj = "EPSG:32617")
sf_grid <- extract_edge_density(sf_grid, jrc_path, categories = c(10, 20), buffer_sizes, 
                                var_name = "Edge_undisturbed_degraded", crs_proj = "EPSG:32617")
write.csv(st_drop_geometry(sf_plots), paste0(backup_dir, "Plots_09_Edge_undisturbed_degraded.csv"), row.names = FALSE)
write.csv(st_drop_geometry(sf_grid), paste0(backup_dir, "Grid_09_Edge_undisturbed_degraded.csv"), row.names = FALSE)
cat("✓ Backup saved\n\n")

# ── EDGE DENSITY (All forest types) ────────────────────────────────────────

cat("\n========== EDGE DENSITY: All forest types (Categories 10+20+30) ==========\n")
sf_plots <- extract_edge_density(sf_plots, jrc_path, categories = c(10, 20, 30), buffer_sizes, 
                                 var_name = "Edge_all_forest", crs_proj = "EPSG:32617")
sf_grid <- extract_edge_density(sf_grid, jrc_path, categories = c(10, 20, 30), buffer_sizes, 
                                var_name = "Edge_all_forest", crs_proj = "EPSG:32617")
write.csv(st_drop_geometry(sf_plots), paste0(backup_dir, "Plots_10_Edge_all_forest.csv"), row.names = FALSE)
write.csv(st_drop_geometry(sf_grid), paste0(backup_dir, "Grid_10_Edge_all_forest.csv"), row.names = FALSE)
cat("✓ Backup saved\n\n")

# ════════════════════════════════════════════════════════════════════════════
# PART 5: FINALIZE AND SAVE OUTPUTS
# ════════════════════════════════════════════════════════════════════════════

cat("\n========== FINALIZING ==========\n\n")

# Convert sf objects to dataframe and save
df_plots_final <- sf_plots %>% st_drop_geometry() %>% as_tibble()
df_grid_final <- sf_grid %>% st_drop_geometry() %>% as_tibble()

write.csv(df_plots_final, "D:/au813514/MKY_Reanalyses/Extracted_Metrics_Plots_FINAL.csv", row.names = FALSE)
write.csv(df_grid_final, "D:/au813514/MKY_Reanalyses/Extracted_Metrics_Grid_FINAL.csv", row.names = FALSE)

cat("✓ Final CSV files saved\n\n")

# ════════════════════════════════════════════════════════════════════════════
# SUMMARY
# ════════════════════════════════════════════════════════════════════════════

cat("\n========== PIPELINE COMPLETE ==========\n\n")
cat(sprintf("Study plots:    %d rows × %d columns\n", nrow(df_plots_final), ncol(df_plots_final)))
cat(sprintf("Prediction grid: %d rows × %d columns\n\n", nrow(df_grid_final), ncol(df_grid_final)))
cat(sprintf("Buffer scales: %s meters\n\n", paste(buffer_sizes, collapse = ", ")))
cat("Extracted metrics:\n")
cat("  ✓ Elevation & Slope (point extraction)\n")
cat("  ✓ Canopy height (CHM v2, mean per buffer)\n")
cat("  ✓ Forest Landscape Integrity Index (FLII, mean per buffer)\n")
cat("  ✓ Vegetation Greenness (NDVI, mean per buffer)\n")
cat("  ✓ Forest cover: undisturbed, degraded, regrowth (% per buffer)\n")
cat("  ✓ Road infrastructure density (m/m² per buffer)\n")
cat("  ✓ Edge density: undisturbed, undisturbed+degraded, all forest (m/m² per buffer)\n")
cat(sprintf("\nBackup files available in: %s\n", backup_dir))
cat("\n✓ All data ready for GAM habitat suitability modeling\n")