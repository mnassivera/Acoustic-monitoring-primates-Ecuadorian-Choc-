# ════════════════════════════════════════════════════════════════════════════
# DATA ANALYSIS
# ════════════════════════════════════════════════════════════════════════════
#
# Project: Acoustic monitoring and habitat suitability of endangered primates
#          in the Ecuadorian Chocó (brown-headed spider monkey and howler monkey)
#
# ════════════════════════════════════════════════════════════════════════════

# ── Libraries ──────────────────────────────────────────────────────────────────

library(openxlsx)      # Read/write Excel files
library(sf)            # Vector spatial data
library(dplyr)         # Data manipulation
library(tidyr)         # Data reshaping
library(stringr)       # String manipulation
library(units)         # Unit handling for spatial calculations

# ════════════════════════════════════════════════════════════════════════════
# IMPORT AND PROCESS ACOUSTIC OBSERVATIONS
# ════════════════════════════════════════════════════════════════════════════

cat("\n========== LOADING ACOUSTIC OBSERVATIONS ==========\n\n")

# Define species list
species_list <- c("Ecuadorian mantled howler monkey", "Brown-headed Spider Monkey")

# Import filtered acoustic observations (already validated)
df_observations <- read.xlsx(
  "C:/Users/nassi/Desktop/Research/Acoustic monitoring reveals different responses of two endangered primates to primary forest degradation in the Ecuadorian Chocó/MKY_Reanalyses/MKY_data_reanalyses.xlsx",
  sheet = "observations_filtered"
) %>%
  filter(is.na(Check) == TRUE) %>%  # Remove manually checked false positives
  select(-Check)

cat(sprintf("✓ Imported %d acoustic observations\n", nrow(df_observations)))

# ────────────────────────────────────────────────────────────────────────────
# Extract plot identifiers from file paths
# ────────────────────────────────────────────────────────────────────────────

df_observations <- df_observations %>%
  mutate(
    # Soundbox_ID: extracted from "/Soundboxes/Plot_XXXX" paths
    Soundbox_ID = if_else(
      str_detect(File, "/Soundboxes/Plot_\\d+"),
      str_extract(File, "Plot_(\\d+)") %>% str_replace("Plot_", ""),
      NA_character_
    ),
    
    # Plot_ID: extracted from REASSEMBLY file paths
    Plot_ID = str_extract(
      File,
      "/data/42-julia-hpc-bio-zoo3/sok73hu/Ecuador/2022/Ecuador2022/Ecuador2022_([^/]+)"
    ) %>%
      str_replace("/data/42-julia-hpc-bio-zoo3/sok73hu/Ecuador/2022/Ecuador2022/Ecuador2022_", "") %>%
      str_replace_all("-", ""),
    
    # Jocotoco_ID: extracted from "/CANANDE/YYYY-MM_YYYY-MM/C_XXXX" paths
    Jocotoco_ID = str_match(
      File,
      "/CANANDE/\\d{4}-\\d{2}_(\\d{4})-\\d{2}/([^/]+)"
    )[, 2:3] %>%
      apply(1, function(x) {
        year <- x[1]
        id <- str_remove(x[2], "^C_")
        paste0(id, "_", year)
      })
  ) %>%
  mutate(
    Soundbox_ID = as.numeric(Soundbox_ID),
    Jocotoco_ID = if_else(Jocotoco_ID == "NA_NA", NA_character_, Jocotoco_ID)
  )

cat("✓ Extracted plot identifiers from file paths\n\n")

# ════════════════════════════════════════════════════════════════════════════
# IMPORT ENVIRONMENTAL DATA
# ════════════════════════════════════════════════════════════════════════════

cat("\n========== LOADING ENVIRONMENTAL DATA ==========\n\n")

# Import study plots with extracted environmental metrics
df_plots <- read.xlsx(
  "C:/Users/nassi/Desktop/Research/Acoustic monitoring reveals different responses of two endangered primates to primary forest degradation in the Ecuadorian Chocó/MKY_Reanalyses/MKY_data_reanalyses.xlsx",
  sheet = "sites description"
) %>%
  filter(!duplicated(Jocotoco_ID) | is.na(Jocotoco_ID)) %>%
  filter(!Jocotoco_ID %in% c(
    "G04_2025", "G22_2025", "G03_2025", "G13_2025",
    "GW62_2025", "G14_2025", "G05_2025", "G28_2025"
  ))

cat(sprintf("✓ Imported %d study plots with environmental metrics\n", nrow(df_plots)))

# Import prediction grid
df_grid <- read.xlsx(
  "C:/Users/nassi/Desktop/Research/Acoustic monitoring reveals different responses of two endangered primates to primary forest degradation in the Ecuadorian Chocó/MKY_Reanalyses/MKY_data_reanalyses.xlsx",
  sheet = "grid description"
)

sf_grid <- st_as_sf(
  df_grid,
  coords = c("Longitude", "Latitude"),
  crs = 4326,
  remove = FALSE
)

cat(sprintf("✓ Imported %d prediction grid points\n\n", nrow(df_grid)))

# ════════════════════════════════════════════════════════════════════════════
# AGGREGATE OBSERVATIONS AND MERGE WITH ENVIRONMENTAL DATA
# ════════════════════════════════════════════════════════════════════════════

cat("\n========== AGGREGATING OBSERVATIONS ==========\n\n")

# Remove redundant columns and aggregate observations
df_observations <- df_observations %>%
  select(-Confidence, -Start, -End) %>%
  distinct() %>%  # Keep only 1 record per audio file
  group_by(Soundbox_ID, Plot_ID, Jocotoco_ID, Common_name) %>%
  summarise(Counts = n(), .groups = "drop")

cat(sprintf("✓ Aggregated observations to %d unique site-species combinations\n", nrow(df_observations)))

# ────────────────────────────────────────────────────────────────────────────
# Create observation tables for each plot type (Soundbox, REASSEMBLY, Jocotoco)
# ────────────────────────────────────────────────────────────────────────────

obs_by_plot <- df_observations %>%
  filter(!is.na(Plot_ID) & is.na(Soundbox_ID) & is.na(Jocotoco_ID)) %>%
  select(-Soundbox_ID, -Jocotoco_ID) %>%
  mutate(
    Plot_ID = as.character(Plot_ID),
    Common_name = as.character(Common_name)
  )

obs_by_soundbox <- df_observations %>%
  filter(!is.na(Soundbox_ID) & is.na(Plot_ID) & is.na(Jocotoco_ID)) %>%
  select(-Plot_ID, -Jocotoco_ID) %>%
  mutate(
    Soundbox_ID = as.character(Soundbox_ID),
    Common_name = as.character(Common_name)
  )

obs_by_jocotoco <- df_observations %>%
  filter(!is.na(Jocotoco_ID) & is.na(Plot_ID) & is.na(Soundbox_ID)) %>%
  select(-Plot_ID, -Soundbox_ID) %>%
  mutate(
    Jocotoco_ID = as.character(Jocotoco_ID),
    Common_name = as.character(Common_name)
  )

# ────────────────────────────────────────────────────────────────────────────
# Merge observations with environmental data (cross-join by species)
# ────────────────────────────────────────────────────────────────────────────

df_data <- df_plots %>%
  mutate(dummy = 1) %>%
  crossing(Common_name = species_list) %>%
  select(-dummy) %>%
  mutate(
    Plot_ID = as.character(Plot_ID),
    Soundbox_ID = as.character(Soundbox_ID),
    Jocotoco_ID = as.character(Jocotoco_ID),
    Common_name = as.character(Common_name)
  )

# Join observations from each monitoring source
df_data <- df_data %>%
  left_join(obs_by_plot, by = c("Plot_ID", "Common_name")) %>%
  left_join(obs_by_soundbox, by = c("Soundbox_ID", "Common_name")) %>%
  left_join(obs_by_jocotoco, by = c("Jocotoco_ID", "Common_name")) %>%
  mutate(Counts = coalesce(Counts.x, Counts.y, Counts)) %>%
  select(-Counts.x, -Counts.y)

# Fill missing counts with 0 (no detection)
df_data <- df_data %>%
  mutate(Counts = if_else(is.na(Counts), 0, Counts))

# Remove Colombian white-faced capuchin (not focal species)
df_data <- df_data %>%
  filter(Common_name != "Colombian white-faced capuchin")

# Consolidate Plot_ID (use non-NA value from either Plot_ID or Jocotoco_ID)
df_data <- df_data %>%
  mutate(Plot_ID = coalesce(Plot_ID, Jocotoco_ID))

# Keep only columns needed for modeling
df_data <- df_data %>%
  select(
    -Jocotoco_ID, -Soundbox_ID, -Study_area, -Matrix,
    -Category, -Regeneration_year
  )

# Clean up temporary objects
rm(obs_by_plot, obs_by_soundbox, obs_by_jocotoco)

cat("✓ Merged observations with environmental data\n\n")

# ════════════════════════════════════════════════════════════════════════════
# CREATE RESPONSE VARIABLES
# ════════════════════════════════════════════════════════════════════════════

cat("\n========== CREATING RESPONSE VARIABLES ==========\n\n")

# Create occurrence/absence variable (binary: 0 = absence, 1 = presence)
df_data <- df_data %>%
  mutate(Occurrence = as.numeric(Counts > 0))

cat("✓ Created Occurrence (presence/absence) variable\n")

# Create scaled coordinates for spatial GAM smooth term
df_data <- df_data %>%
  mutate(
    Latitude_scaled = scale(Latitude)[, 1],
    Longitude_scaled = scale(Longitude)[, 1]
  )

df_grid <- df_grid %>%
  mutate(
    Latitude_scaled = scale(Latitude)[, 1],
    Longitude_scaled = scale(Longitude)[, 1]
  )

cat("✓ Scaled geographic coordinates for spatial smoothing\n\n")

# ════════════════════════════════════════════════════════════════════════════
# RESOLVE SPATIAL PSEUDO-REPLICATION
# ════════════════════════════════════════════════════════════════════════════

cat("\n========== DEDUPLICATING OBSERVATIONS ==========\n\n")

# Separate species for independent deduplication
df_data_spider_monkey <- df_data %>%
  filter(Common_name == "Brown-headed Spider Monkey")

df_data_howler_monkey <- df_data %>%
  filter(Common_name == "Ecuadorian mantled howler monkey")

cat(sprintf("Spider monkey: %d initial observations\n", nrow(df_data_spider_monkey)))
cat(sprintf("Howler monkey: %d initial observations\n\n", nrow(df_data_howler_monkey)))

# ────────────────────────────────────────────────────────────────────────────
# Function 1: Deduplicate by grid cell (keep max counts per grid cell)
# ────────────────────────────────────────────────────────────────────────────

deduplicate_by_grid <- function(df_data_species) {
  
  # Convert to sf for spatial operations
  sf_data_species <- st_as_sf(
    df_data_species,
    coords = c("Longitude", "Latitude"),
    crs = 4326,
    remove = FALSE
  )
  
  # Spatial join: assign each observation to nearest grid cell
  sf_grid_subset <- sf_grid %>%
    select(Grid_ID, geometry)
  
  df_joined <- st_join(
    sf_data_species,
    sf_grid_subset,
    join = st_nearest_feature
  ) %>%
    st_drop_geometry()
  
  # Keep only the highest-count observation per grid cell
  df_dedup <- df_joined %>%
    group_by(Grid_ID) %>%
    slice_max(order_by = Counts, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  cat(sprintf("  Grid deduplication: %d → %d observations\n",
              nrow(df_data_species), nrow(df_dedup)))
  
  return(df_dedup)
}

df_data_spider_monkey <- deduplicate_by_grid(df_data_spider_monkey)
df_data_howler_monkey <- deduplicate_by_grid(df_data_howler_monkey)

cat("\n")

# ────────────────────────────────────────────────────────────────────────────
# Function 2: Deduplicate by distance (keep points ≥500m apart)
# ────────────────────────────────────────────────────────────────────────────

deduplicate_by_distance <- function(df_data_species, distance_m = 500) {
  
  distance <- units::set_units(distance_m, "m")
  
  # Separate zero and non-zero counts
  df_zeros <- df_data_species %>%
    filter(Counts == 0)
  
  df_nonzeros <- df_data_species %>%
    filter(Counts > 0)
  
  # Convert positive counts to sf and project to UTM for distance calculations
  sf_nonzeros <- st_as_sf(
    df_nonzeros,
    coords = c("Longitude", "Latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
    st_transform(crs = 32717)  # UTM zone 17S
  
  # Greedy algorithm: iteratively add points that are ≥distance_m from all kept points
  kept_indices <- c()
  
  for (i in 1:nrow(sf_nonzeros)) {
    if (length(kept_indices) == 0) {
      kept_indices <- c(kept_indices, i)
    } else {
      kept_so_far <- sf_nonzeros[kept_indices, ]
      current_point <- sf_nonzeros[i, ]
      distances <- st_distance(current_point, kept_so_far)
      
      if (all(distances > distance)) {
        kept_indices <- c(kept_indices, i)
      }
    }
  }
  
  # Extract deduplicated non-zeros and recombine with zeros
  df_nonzeros_dedup <- sf_nonzeros[kept_indices, ] %>%
    st_drop_geometry()
  
  df_dedup <- bind_rows(df_zeros, df_nonzeros_dedup)
  
  cat(sprintf("  Distance deduplication (≥%d m apart):\n", distance_m))
  cat(sprintf("    Original points: %d\n", nrow(df_data_species)))
  cat(sprintf("    Zero counts: %d (retained)\n", nrow(df_zeros)))
  cat(sprintf("    Positive counts: %d → %d (deduplicated)\n",
              nrow(df_nonzeros), nrow(df_nonzeros_dedup)))
  cat(sprintf("    Final dataset: %d points\n", nrow(df_dedup)))
  
  return(df_dedup)
}

df_data_spider_monkey <- deduplicate_by_distance(df_data_spider_monkey, distance_m = 500)
cat("\n")
df_data_howler_monkey <- deduplicate_by_distance(df_data_howler_monkey, distance_m = 500)
cat("\n")

# ════════════════════════════════════════════════════════════════════════════
# SUMMARY AND VERIFICATION
# ════════════════════════════════════════════════════════════════════════════

cat("\n========== DATA PREPARATION COMPLETE ==========\n\n")

cat("Spider monkey (Ateles fusciceps fusciceps):\n")
cat(sprintf("  %d observations\n", nrow(df_data_spider_monkey)))
cat(sprintf("  %d sites with presence (Occurrence=1)\n",
            sum(df_data_spider_monkey$Occurrence)))
cat(sprintf("  %d sites with absence (Occurrence=0)\n",
            sum(df_data_spider_monkey$Occurrence == 0)))
cat(sprintf("  Detection counts: mean=%.2f, min=%d, max=%d\n",
            mean(df_data_spider_monkey$Counts),
            min(df_data_spider_monkey$Counts),
            max(df_data_spider_monkey$Counts)))

cat("\n")

cat("Howler monkey (Alouatta palliata aequatorialis):\n")
cat(sprintf("  %d observations\n", nrow(df_data_howler_monkey)))
cat(sprintf("  %d sites with presence (Occurrence=1)\n",
            sum(df_data_howler_monkey$Occurrence)))
cat(sprintf("  %d sites with absence (Occurrence=0)\n",
            sum(df_data_howler_monkey$Occurrence == 0)))
cat(sprintf("  Detection counts: mean=%.2f, min=%d, max=%d\n",
            mean(df_data_howler_monkey$Counts),
            min(df_data_howler_monkey$Counts),
            max(df_data_howler_monkey$Counts)))

cat("\n✓ Datasets ready for habitat suitability modeling\n")

# ════════════════════════════════════════════════════════════════════════════
# CREATE FOREST_TOTAL COLUMNS (in both datasets)
# ════════════════════════════════════════════════════════════════════════════

library(dplyr)
library(mgcv)
library(tidyr)
library(openxlsx)

buffer_sizes <- c(100, 200, 500, 1000, 2000)

# Create Forest_total for each buffer size in df_data_spider_monkey
df_data_spider_monkey <- df_data_spider_monkey %>%
  mutate(
    Forest_total_100m = Forest_undisturbed_100m + Forest_degraded_100m + Forest_regrowth_100m,
    Forest_total_200m = Forest_undisturbed_200m + Forest_degraded_200m + Forest_regrowth_200m,
    Forest_total_500m = Forest_undisturbed_500m + Forest_degraded_500m + Forest_regrowth_500m,
    Forest_total_1000m = Forest_undisturbed_1000m + Forest_degraded_1000m + Forest_regrowth_1000m,
    Forest_total_2000m = Forest_undisturbed_2000m + Forest_degraded_2000m + Forest_regrowth_2000m
  )

# Create Forest_total for each buffer size in df_data_howler_monkey
df_data_howler_monkey <- df_data_howler_monkey %>%
  mutate(
    Forest_total_100m = Forest_undisturbed_100m + Forest_degraded_100m + Forest_regrowth_100m,
    Forest_total_200m = Forest_undisturbed_200m + Forest_degraded_200m + Forest_regrowth_200m,
    Forest_total_500m = Forest_undisturbed_500m + Forest_degraded_500m + Forest_regrowth_500m,
    Forest_total_1000m = Forest_undisturbed_1000m + Forest_degraded_1000m + Forest_regrowth_1000m,
    Forest_total_2000m = Forest_undisturbed_2000m + Forest_degraded_2000m + Forest_regrowth_2000m
  )

# Create Forest_total for each buffer size in df_grid
df_grid <- df_grid %>%
  mutate(
    Forest_total_100m = Forest_undisturbed_100m + Forest_degraded_100m + Forest_regrowth_100m,
    Forest_total_200m = Forest_undisturbed_200m + Forest_degraded_200m + Forest_regrowth_200m,
    Forest_total_500m = Forest_undisturbed_500m + Forest_degraded_500m + Forest_regrowth_500m,
    Forest_total_1000m = Forest_undisturbed_1000m + Forest_degraded_1000m + Forest_regrowth_1000m,
    Forest_total_2000m = Forest_undisturbed_2000m + Forest_degraded_2000m + Forest_regrowth_2000m
  )

cat("✓ Created Forest_total_*m columns (sum of undisturbed + degraded + regrowth)\n\n")

# ════════════════════════════════════════════════════════════════════════════
# CONVERT EDGE DENSITY AND ROAD DENSITY FROM km/km² TO km/ha
# ════════════════════════════════════════════════════════════════════════════

# Convert edge density in df_data_spider_monkey (multiply by 100)
df_data_spider_monkey <- df_data_spider_monkey %>%
  mutate(
    across(starts_with("Edge_"), ~ . * 100)
  )

# Convert edge density in df_data_howler_monkey (multiply by 100)
df_data_howler_monkey <- df_data_howler_monkey %>%
  mutate(
    across(starts_with("Edge_"), ~ . * 100)
  )

# Convert edge density in df_grid (multiply by 100)
df_grid <- df_grid %>%
  mutate(
    across(starts_with("Edge_"), ~ . * 100)
  )

cat("✓ Converted edge density from km/km² to km/ha (multiplied by 100)\n\n")

# ════════════════════════════════════════════════════════════════════════════
# CONVERT ROAD DENSITY FROM m/m² TO km/ha
# ════════════════════════════════════════════════════════════════════════════

# Convert road density in df_data_spider_monkey (multiply by 100)
df_data_spider_monkey <- df_data_spider_monkey %>%
  mutate(
    across(starts_with("Road_density_"), ~ . * 100)
  )

# Convert road density in df_data_howler_monkey (multiply by 100)
df_data_howler_monkey <- df_data_howler_monkey %>%
  mutate(
    across(starts_with("Road_density_"), ~ . * 100)
  )

# Convert road density in df_grid (multiply by 100)
df_grid <- df_grid %>%
  mutate(
    across(starts_with("Road_density_"), ~ . * 100)
  )

cat("✓ Converted road density from m/m² to km/ha (multiplied by 100)\n\n")

# ════════════════════════════════════════════════════════════════════════════
# SCALE OF EFFECT ANALYSIS - OCCURRENCE ONLY
# ════════════════════════════════════════════════════════════════════════════

# Helper function for binomial GAMs
fit_gam_binomial <- function(vars, data, response) {
  aic_values <- numeric(length(vars))
  adj_r2_values <- numeric(length(vars))
  names(aic_values) <- vars
  names(adj_r2_values) <- vars
  
  for (v in vars) {
    f <- as.formula(paste0(response, " ~ ", v))
    model <- gam(f, data = data, family = binomial(link = "logit"), select = TRUE)
    sm <- summary(model)
    aic_values[v] <- AIC(model)
    adj_r2_values[v] <- sm$r.sq
  }
  
  tibble(
    Variable = vars,
    AIC = round(aic_values, 2),
    Adj_R2 = round(adj_r2_values, 4)
  )
}

# Define variable groups
variable_groups <- list(
  Topography = c("Elevation", "Slope"),
  Canopy_height = paste0("Canopy_height_", buffer_sizes, "m"),
  Integrity = paste0("Integrity_", buffer_sizes, "m"),
  NDVI = paste0("NDVI_", buffer_sizes, "m"),
  Forest_undisturbed = paste0("Forest_undisturbed_", buffer_sizes, "m"),
  Forest_degraded = paste0("Forest_degraded_", buffer_sizes, "m"),
  Forest_regrowth = paste0("Forest_regrowth_", buffer_sizes, "m"),
  Forest_total = paste0("Forest_total_", buffer_sizes, "m"),
  Road_density = paste0("Road_density_", buffer_sizes, "m"),
  Edge_undisturbed = paste0("Edge_undisturbed_", buffer_sizes, "m"),
  Edge_undisturbed_degraded = paste0("Edge_undisturbed_degraded_", buffer_sizes, "m"),
  Edge_all_forest = paste0("Edge_all_forest_", buffer_sizes, "m")
)

# ────────────────────────────────────────────────────────────────────────────
# SPIDER MONKEY - OCCURRENCE (Binomial)
# ────────────────────────────────────────────────────────────────────────────

cat("Analyzing: Spider monkey Occurrence (binomial)\n")

results_spider_occ <- lapply(names(variable_groups), function(group_name) {
  vars <- variable_groups[[group_name]]
  available_vars <- vars[vars %in% colnames(df_data_spider_monkey)]
  
  if (length(available_vars) == 0) return(NULL)
  
  result <- fit_gam_binomial(available_vars, df_data_spider_monkey, "Occurrence")
  result$Variable_group <- group_name
  result
}) %>%
  bind_rows() %>%
  arrange(Variable_group, AIC) %>%
  mutate(Delta_AIC = round(AIC - min(AIC, na.rm = TRUE), 2), .by = Variable_group) %>%
  select(Variable_group, Variable, AIC, Delta_AIC, Adj_R2)

cat("✓ Spider monkey Occurrence complete\n")

# ────────────────────────────────────────────────────────────────────────────
# HOWLER MONKEY - OCCURRENCE (Binomial)
# ────────────────────────────────────────────────────────────────────────────

cat("Analyzing: Howler monkey Occurrence (binomial)\n")

results_howler_occ <- lapply(names(variable_groups), function(group_name) {
  vars <- variable_groups[[group_name]]
  available_vars <- vars[vars %in% colnames(df_data_howler_monkey)]
  
  if (length(available_vars) == 0) return(NULL)
  
  result <- fit_gam_binomial(available_vars, df_data_howler_monkey, "Occurrence")
  result$Variable_group <- group_name
  result
}) %>%
  bind_rows() %>%
  arrange(Variable_group, AIC) %>%
  mutate(Delta_AIC = round(AIC - min(AIC, na.rm = TRUE), 2), .by = Variable_group) %>%
  select(Variable_group, Variable, AIC, Delta_AIC, Adj_R2)

cat("✓ Howler monkey Occurrence complete\n")

# ────────────────────────────────────────────────────────────────────────────
# EXPORT TO EXCEL
# ────────────────────────────────────────────────────────────────────────────
output_file <- "D:/au813514/MKY_Reanalyses/Scale_of_Effect_Analysis.xlsx"

wb <- createWorkbook()

# Sheet 1: Spider monkey Occurrence
addWorksheet(wb, "Spider_Occurrence")
writeData(wb, "Spider_Occurrence", results_spider_occ)
setColWidths(wb, "Spider_Occurrence", cols = 1:5, widths = c(18, 25, 10, 12, 10))

# Sheet 2: Howler monkey Occurrence
addWorksheet(wb, "Howler_Occurrence")
writeData(wb, "Howler_Occurrence", results_howler_occ)
setColWidths(wb, "Howler_Occurrence", cols = 1:5, widths = c(18, 25, 10, 12, 10))

# Save workbook
saveWorkbook(wb, output_file, overwrite = TRUE)

cat(sprintf("\n✓ Scale-of-effect analysis exported to:\n  %s\n", output_file))
cat("✓ 2 sheets created:\n")
cat("  - Spider_Occurrence (AIC-based, binomial)\n")
cat("  - Howler_Occurrence (AIC-based, binomial)\n\n")

# ────────────────────────────────────────────────────────────────────────────
# SUMMARY: BEST BUFFER PER VARIABLE GROUP
# ────────────────────────────────────────────────────────────────────────────

cat("\n========== SUMMARY: BEST BUFFER SIZES ==========\n\n")

summary_spider_occ <- results_spider_occ %>%
  slice_min(AIC, by = Variable_group) %>%
  mutate(Species = "Spider") %>%
  select(Species, Variable_group, Variable, AIC, Adj_R2)

summary_howler_occ <- results_howler_occ %>%
  slice_min(AIC, by = Variable_group) %>%
  mutate(Species = "Howler") %>%
  select(Species, Variable_group, Variable, AIC, Adj_R2)

summary_all <- bind_rows(
  summary_spider_occ,
  summary_howler_occ
) %>%
  arrange(Species, Variable_group)

print(summary_all)

cat("\n✓ Analysis complete!\n")

# ════════════════════════════════════════════════════════════════════════════
# GAM MODEL SELECTION FOR SPIDER MONKEY - DREDGE-BASED
# Uses native dredge subset argument for multicollinearity constraints
# ════════════════════════════════════════════════════════════════════════════

library(mgcv)
library(MuMIn)
library(dplyr)
library(tidyr)
library(openxlsx)
library(sf)

cat("\n========== SPIDER MONKEY - MODEL SELECTION ==========\n\n")

# ════════════════════════════════════════════════════════════════════════════
# READ SCALE-OF-EFFECT RESULTS
# ════════════════════════════════════════════════════════════════════════════

# Load scale-of-effect results
scale_results <- summary_spider_occ

# Extract best buffer (lowest AIC) for each variable group
best_buffers <- scale_results %>%
  filter(!is.na(AIC)) %>%
  slice_min(AIC, by = Variable_group) %>%
  select(Variable_group, Variable) %>%
  deframe()

cat("Best buffer sizes:\n")
for (group in names(best_buffers)) {
  cat(sprintf("  %s → %s\n", group, best_buffers[[group]]))
}
cat("\n")

# ════════════════════════════════════════════════════════════════════════════
# SELECT PREDICTORS (include both single & total versions)
# ════════════════════════════════════════════════════════════════════════════

predictor_vars <- c(
  "Elevation",
  "Slope",
  best_buffers[["Canopy_height"]],
  best_buffers[["Integrity"]],
  best_buffers[["NDVI"]],
  best_buffers[["Forest_undisturbed"]],
  best_buffers[["Forest_total"]],
  best_buffers[["Road_density"]],
  best_buffers[["Edge_undisturbed"]],
  best_buffers[["Edge_all_forest"]]
)

predictor_vars <- unique(predictor_vars)
available_vars <- predictor_vars[predictor_vars %in% colnames(df_data_spider_monkey)]

cat(sprintf("Selected %d predictors:\n", length(available_vars)))
for (var in available_vars) {
  cat(sprintf("  ✓ %s\n", var))
}
cat("\n")

# ════════════════════════════════════════════════════════════════════════════
# PREPARE DATA AND COMPUTE CORRELATION MATRIX
# ════════════════════════════════════════════════════════════════════════════

df_spider <- df_data_spider_monkey %>%
  st_drop_geometry() %>%
  select(Occurrence, Latitude_scaled, Longitude_scaled, all_of(available_vars))

cat(sprintf("Data: n = %d | Occurrences: %d (%.1f%%)\n\n",
            nrow(df_spider),
            sum(df_spider$Occurrence),
            100 * mean(df_spider$Occurrence)))

# Compute correlation matrix for subset constraint
data_numeric <- df_spider %>% select(all_of(available_vars))
cor_mat <- cor(data_numeric, use = "pairwise.complete.obs")

# Find high correlation pairs
high_cor_pairs <- data.frame()
for (i in 1:(nrow(cor_mat) - 1)) {
  for (j in (i + 1):nrow(cor_mat)) {
    if (abs(cor_mat[i, j]) > 0.70) {
      high_cor_pairs <- rbind(high_cor_pairs, data.frame(
        Var1 = rownames(cor_mat)[i],
        Var2 = colnames(cor_mat)[j],
        Correlation = round(cor_mat[i, j], 3)
      ))
    }
  }
}

if (nrow(high_cor_pairs) > 0) {
  cat("Multicollinearity detected (|r| > 0.70):\n")
  print(high_cor_pairs)
  cat("\nThese pairs will be excluded from candidate models.\n\n")
} else {
  cat("✓ No multicollinearity detected\n\n")
}

# ════════════════════════════════════════════════════════════════════════════
# BUILD GLOBAL MODEL
# ════════════════════════════════════════════════════════════════════════════

pred_formula <- paste(available_vars, collapse = " + ")
global_formula <- as.formula(
  paste("Occurrence ~", pred_formula, 
        "+ s(Latitude_scaled, Longitude_scaled, bs='tp', k=10)")
)

cat("Fitting global model...\n")
gam_spider_global <- gam(
  global_formula,
  data = df_spider,
  family = binomial(link = "logit"),
  na.action = na.fail,
  select = TRUE
)

cat("✓ Global model AIC:", round(AIC(gam_spider_global), 2), "\n")
cat("✓ Deviance explained:", 
    round(100 * summary(gam_spider_global)$dev.expl, 2), "%\n\n")

# ════════════════════════════════════════════════════════════════════════════
# CREATE SUBSET CONSTRAINT FOR DREDGE
# ════════════════════════════════════════════════════════════════════════════

# Build expression to exclude multicollinear pairs
# Example: !(Var1 & Var2) means "do NOT include both Var1 and Var2"

if (nrow(high_cor_pairs) > 0) {
  subset_expr <- character()
  for (i in 1:nrow(high_cor_pairs)) {
    var1 <- high_cor_pairs$Var1[i]
    var2 <- high_cor_pairs$Var2[i]
    subset_expr <- c(subset_expr, 
                     sprintf("!(`%s` & `%s`)", var1, var2))
  }
  
  # Combine all constraints with AND (&)
  subset_formula <- as.formula(paste("~", paste(subset_expr, collapse = " & ")))
  
  cat("Subset constraints created for dredge:\n")
  for (expr in subset_expr) {
    cat(sprintf("  %s\n", expr))
  }
  cat("\n")
} else {
  subset_formula <- NULL
}

# ════════════════════════════════════════════════════════════════════════════
# RUN DREDGE WITH MULTICOLLINEARITY CONSTRAINTS
# ════════════════════════════════════════════════════════════════════════════

cat("Running dredge (max 4 predictors + spatial smooth)...\n")
dredge_spider <- dredge(
  gam_spider_global,
  rank = "AIC",
  m.lim = c(0, 4),
  subset = subset_formula,
  trace = FALSE
)
cat("✓ Dredge complete\n")
cat(sprintf("  Candidate models: %d\n", nrow(dredge_spider)))
best_spider <- dredge_spider[dredge_spider$delta <= 2, ]
cat(sprintf("  Best models (ΔAICc ≤ 2): %d\n\n", nrow(best_spider)))
print(best_spider)

# ════════════════════════════════════════════════════════════════════════════
# EXTRACT P-VALUES FROM BEST MODELS
# ════════════════════════════════════════════════════════════════════════════

selected_models <- get.models(dredge_spider, subset = delta <= 2)

# Extract p-values and estimates for each model
pval_list <- lapply(seq_along(selected_models), function(i) {
  m <- selected_models[[i]]
  coef_table <- summary(m)$p.table
  
  data.frame(
    Model_ID = i,
    Variable = rownames(coef_table),
    Estimate = round(coef_table[, "Estimate"], 4),
    Std.Error = round(coef_table[, "Std. Error"], 4),
    z.value = round(coef_table[, "z value"], 4),
    p.value = round(coef_table[, "Pr(>|z|)"], 4)
  )
})

pval_df <- bind_rows(pval_list) %>%
  filter(!Variable %in% c("(Intercept)")) %>%
  arrange(Model_ID, p.value)

cat("✓ Extracted p-values from best models\n\n")
print(pval_df)

# ════════════════════════════════════════════════════════════════════════════
# EXPORT TO EXCEL
# ════════════════════════════════════════════════════════════════════════════

output_file <- "C:/Users/nassi/Desktop/Research/Acoustic monitoring reveals different responses of two endangered primates to primary forest degradation in the Ecuadorian Chocó/MKY_Reanalyses/Spider_Monkey_Model_Selection.xlsx"

wb <- createWorkbook()

# All models
addWorksheet(wb, "All_Models")
writeData(wb, "All_Models", as.data.frame(dredge_spider))
setColWidths(wb, "All_Models", cols = 1:min(15, ncol(dredge_spider)), widths = "auto")

# Best models
addWorksheet(wb, "Best_Models")
writeData(wb, "Best_Models", as.data.frame(best_spider))
setColWidths(wb, "Best_Models", cols = 1:min(15, ncol(best_spider)), widths = "auto")

# P-values and coefficients
addWorksheet(wb, "Best_Models_Pvalues")
writeData(wb, "Best_Models_Pvalues", pval_df)
setColWidths(wb, "Best_Models_Pvalues", cols = 1:6, widths = c(12, 25, 12, 12, 10, 10))

# High correlations
if (nrow(high_cor_pairs) > 0) {
  addWorksheet(wb, "Excluded_Pairs")
  writeData(wb, "Excluded_Pairs", high_cor_pairs)
  setColWidths(wb, "Excluded_Pairs", cols = 1:3, widths = c(25, 25, 15))
}

# Metadata
metadata <- data.frame(
  Item = c(
    "Sample size",
    "Occurrences (n)",
    "Occurrences (%)",
    "Number of predictors",
    "Global model AIC",
    "Global model deviance explained (%)",
    "Spatial smooth",
    "Max linear predictors per model",
    "Multicollinearity threshold",
    "Best models criterion"
  ),
  Value = c(
    nrow(df_spider),
    sum(df_spider$Occurrence),
    sprintf("%.1f", 100 * mean(df_spider$Occurrence)),
    length(available_vars),
    round(AIC(gam_spider_global), 2),
    round(100 * summary(gam_spider_global)$dev.expl, 2),
    "s(Latitude_scaled, Longitude_scaled, bs='tp', k=10)",
    "4",
    "|r| > 0.70",
    "ΔAICc ≤ 2"
  )
)

addWorksheet(wb, "Metadata")
writeData(wb, "Metadata", metadata)
setColWidths(wb, "Metadata", cols = 1:2, widths = c(35, 30))

saveWorkbook(wb, output_file, overwrite = TRUE)

cat(sprintf("\n✓ Results saved to:\n  %s\n\n", output_file))
cat("✓ Sheets created:\n")
cat("  - All_Models (all candidate models)\n")
cat("  - Best_Models (ΔAICc ≤ 2)\n")
cat("  - Best_Models_Pvalues (p-values and coefficients for each best model)\n")
if (nrow(high_cor_pairs) > 0) {
  cat("  - Excluded_Pairs (multicollinear pairs)\n")
}
cat("  - Metadata (analysis summary)\n\n")
cat("✓ Analysis complete!\n")

# ════════════════════════════════════════════════════════════════════════════
# MODEL AVERAGING
# ════════════════════════════════════════════════════════════════════════════

selected_models <- get.models(dredge_spider, subset = delta <= 2)
selected_weights <- best_spider$weight / sum(best_spider$weight)

cat(sprintf("✓ Averaging predictions from %d models\n\n", length(selected_models)))

# ════════════════════════════════════════════════════════════════════════════
# PARTIAL EFFECTS OF TOP PREDICTORS
# ════════════════════════════════════════════════════════════════════════════

# Extract top 4 predictors by frequency across best models
var_importance <- best_spider %>%
  as.data.frame() %>%
  select(-delta, -weight, -AIC, -df, -logLik) %>%
  select(where(~ !all(is.na(.)))) %>%
  summarise(across(everything(), ~ mean(!is.na(.)))) %>%
  tidyr::pivot_longer(everything(), names_to = "Variable", values_to = "Importance") %>%
  filter(!grepl("Intercept|Latitude_scaled|Longitude_scaled", Variable)) %>%
  arrange(desc(Importance)) %>%
  slice_head(n = 4)

top4_vars <- var_importance$Variable
cat("Top 4 predictors:\n")
print(var_importance)

# Plot function: average predictions across models containing the variable
# Variable name → display label + unit conversion
var_labels <- list(
  Edge_all_forest_500m    = list(label = "Total forest edge\ndensity (km/ha, 500m)"),
  Edge_undisturbed_500m   = list(label = "Undisturbed forest edge\ndensity (km/ha, 500m)"),
  Forest_undisturbed_500m = list(label = "Undisturbed forest\ncover (%, 500m)")
)

plot_partial_effect <- function(var, raw_data) {
  
  info <- var_labels[[var]]
  x_label <- if (!is.null(info$label)) info$label else var
  
  pred_list <- lapply(selected_models, function(m) {
    tryCatch(
      ggpredict(m, terms = paste0(var, " [all]")),
      error = function(e) NULL
    )
  })
  pred_list <- Filter(Negate(is.null), pred_list)
  
  if (length(pred_list) == 0) {
    cat(sprintf("Warning: %s not found in any model\n", var))
    return(NULL)
  }
  
  pred_avg <- pred_list[[1]]
  if (length(pred_list) > 1) {
    pred_avg$predicted <- rowMeans(sapply(pred_list, `[[`, "predicted"))
    pred_avg$conf.low  <- rowMeans(sapply(pred_list, `[[`, "conf.low"))
    pred_avg$conf.high <- rowMeans(sapply(pred_list, `[[`, "conf.high"))
  }

  ggplot(pred_avg, aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey") +
    geom_line(color = "black", linewidth = 4) +
    geom_point(data = raw_data, aes(x = .data[[var]], y = Occurrence),
               alpha = 0.3, size = 7, color = "black") +
    labs(x = x_label, y = "Species occurrence") +
    theme_classic() +
    theme(
      aspect.ratio = 1,
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.title = element_text(size = 35),
      axis.text = element_text(size = 25)
    )
}

# Generate and print plots
plots <- lapply(names(var_labels), function(v) plot_partial_effect(v, df_spider))
for (p in plots) if (!is.null(p)) print(p)

# ════════════════════════════════════════════════════════════════════════════
# MODEL-AVERAGED HABITAT SUITABILITY PREDICTIONS
# ════════════════════════════════════════════════════════════════════════════

pred_matrix <- sapply(selected_models, function(m) {
  predict(m, newdata = df_grid, type = "response")
})

df_grid$HSM_spider_occurrence <- as.vector(pred_matrix %*% selected_weights)

# ── Habitat Suitability Map ─────────────────────────────────────────────────

df_grid_sf <- st_as_sf(df_grid, coords = c("Longitude", "Latitude"), crs = 4326)

palette_HSM <- c("#17283B", "#233B57", "#2A5779", "#3880A9", "#5093A3",
                 "#70A49F", "#8FB7A5", "#ABC9AD", "#D3E1CC", "#EDF1E4")

ggplot(df_grid_sf) +
  geom_sf(aes(color = HSM_spider_occurrence), size = 0.65, shape = 15) +
  scale_color_gradientn(
    colours = palette_HSM, limits = c(0, 1),
    name = "Habitat suitability   "
  ) +
  coord_sf(expand = FALSE) +
  scale_x_continuous(
    breaks = seq(
      floor(min(st_coordinates(df_grid_sf)[, 1])),
      ceiling(max(st_coordinates(df_grid_sf)[, 1])),
      by = 0.09
    )
  ) +
  guides(
    color = guide_colorbar(
      barheight = unit(1.1, "cm"), barwidth = unit(15, "cm"),
      ticks = TRUE, frame.colour = "black", frame.linewidth = 1
    )
  ) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
    legend.key = element_rect(colour = "black", linewidth = 1),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 25),
    legend.position = "bottom"
  ) +
  labs(
    x = "Longitude", y = "Latitude"
  )

# ════════════════════════════════════════════════════════════════════════════
# GAM MODEL SELECTION FOR HOWLER MONKEY - DREDGE-BASED
# Uses native dredge subset argument for multicollinearity constraints
# ════════════════════════════════════════════════════════════════════════════

library(mgcv)
library(MuMIn)
library(dplyr)
library(tidyr)
library(openxlsx)
library(sf)
library(ggplot2)
library(ggeffects)

cat("\n========== HOWLER MONKEY - MODEL SELECTION ==========\n\n")

# ════════════════════════════════════════════════════════════════════════════
# READ SCALE-OF-EFFECT RESULTS
# ════════════════════════════════════════════════════════════════════════════

# Load scale-of-effect results
scale_results <- summary_howler_occ

# Extract best buffer (lowest AIC) for each variable group
best_buffers <- scale_results %>%
  filter(!is.na(AIC)) %>%
  slice_min(AIC, by = Variable_group) %>%
  select(Variable_group, Variable) %>%
  deframe()

cat("Best buffer sizes:\n")
for (group in names(best_buffers)) {
  cat(sprintf("  %s → %s\n", group, best_buffers[[group]]))
}
cat("\n")

# ════════════════════════════════════════════════════════════════════════════
# SELECT PREDICTORS (include both single & total versions)
# ════════════════════════════════════════════════════════════════════════════

predictor_vars <- c(
  "Elevation",
  "Slope",
  best_buffers[["Canopy_height"]],
  best_buffers[["Integrity"]],
  best_buffers[["NDVI"]],
  best_buffers[["Forest_undisturbed"]],
  best_buffers[["Forest_total"]],
  best_buffers[["Road_density"]],
  best_buffers[["Edge_undisturbed"]],
  best_buffers[["Edge_all_forest"]]
)

predictor_vars <- unique(predictor_vars)
available_vars <- predictor_vars[predictor_vars %in% colnames(df_data_howler_monkey)]

cat(sprintf("Selected %d predictors:\n", length(available_vars)))
for (var in available_vars) {
  cat(sprintf("  ✓ %s\n", var))
}
cat("\n")

# ════════════════════════════════════════════════════════════════════════════
# PREPARE DATA AND COMPUTE CORRELATION MATRIX
# ════════════════════════════════════════════════════════════════════════════

df_howler <- df_data_howler_monkey %>%
  st_drop_geometry() %>%
  select(Occurrence, Latitude_scaled, Longitude_scaled, all_of(available_vars))

cat(sprintf("Data: n = %d | Occurrences: %d (%.1f%%)\n\n",
            nrow(df_howler),
            sum(df_howler$Occurrence),
            100 * mean(df_howler$Occurrence)))

# Compute correlation matrix for subset constraint
data_numeric <- df_howler %>% select(all_of(available_vars))
cor_mat <- cor(data_numeric, use = "pairwise.complete.obs")

# Find high correlation pairs
high_cor_pairs <- data.frame()
for (i in 1:(nrow(cor_mat) - 1)) {
  for (j in (i + 1):nrow(cor_mat)) {
    if (abs(cor_mat[i, j]) > 0.70) {
      high_cor_pairs <- rbind(high_cor_pairs, data.frame(
        Var1 = rownames(cor_mat)[i],
        Var2 = colnames(cor_mat)[j],
        Correlation = round(cor_mat[i, j], 3)
      ))
    }
  }
}

if (nrow(high_cor_pairs) > 0) {
  cat("Multicollinearity detected (|r| > 0.70):\n")
  print(high_cor_pairs)
  cat("\nThese pairs will be excluded from candidate models.\n\n")
} else {
  cat("✓ No multicollinearity detected\n\n")
}

# ════════════════════════════════════════════════════════════════════════════
# BUILD GLOBAL MODEL
# ════════════════════════════════════════════════════════════════════════════

pred_formula <- paste(available_vars, collapse = " + ")
global_formula <- as.formula(
  paste("Occurrence ~", pred_formula, 
        "+ s(Latitude_scaled, Longitude_scaled, bs='tp', k=10)")
)

cat("Fitting global model...\n")
gam_howler_global <- gam(
  global_formula,
  data = df_howler,
  family = binomial(link = "logit"),
  na.action = na.fail,
  select = TRUE
)

cat("✓ Global model AIC:", round(AIC(gam_howler_global), 2), "\n")
cat("✓ Deviance explained:", 
    round(100 * summary(gam_howler_global)$dev.expl, 2), "%\n\n")

# ════════════════════════════════════════════════════════════════════════════
# CREATE SUBSET CONSTRAINT FOR DREDGE
# ════════════════════════════════════════════════════════════════════════════

if (nrow(high_cor_pairs) > 0) {
  subset_expr <- character()
  for (i in 1:nrow(high_cor_pairs)) {
    var1 <- high_cor_pairs$Var1[i]
    var2 <- high_cor_pairs$Var2[i]
    subset_expr <- c(subset_expr, 
                     sprintf("!(`%s` & `%s`)", var1, var2))
  }
  
  subset_formula <- as.formula(paste("~", paste(subset_expr, collapse = " & ")))
  
  cat("Subset constraints created for dredge:\n")
  for (expr in subset_expr) {
    cat(sprintf("  %s\n", expr))
  }
  cat("\n")
} else {
  subset_formula <- NULL
}

# ════════════════════════════════════════════════════════════════════════════
# RUN DREDGE WITH MULTICOLLINEARITY CONSTRAINTS
# ════════════════════════════════════════════════════════════════════════════

cat("Running dredge (max 4 predictors + spatial smooth)...\n")
dredge_howler <- dredge(
  gam_howler_global,
  rank = "AIC",
  m.lim = c(0, 4),
  subset = subset_formula,
  trace = FALSE
)
cat("✓ Dredge complete\n")
cat(sprintf("  Candidate models: %d\n", nrow(dredge_howler)))
best_howler <- dredge_howler[dredge_howler$delta <= 2, ]
cat(sprintf("  Best models (ΔAICc ≤ 2): %d\n\n", nrow(best_howler)))
print(best_howler)

# ════════════════════════════════════════════════════════════════════════════
# EXTRACT P-VALUES FROM BEST MODELS
# ════════════════════════════════════════════════════════════════════════════

selected_models <- get.models(dredge_howler, subset = delta <= 2)

# Extract p-values and estimates for each model
pval_list <- lapply(seq_along(selected_models), function(i) {
  m <- selected_models[[i]]
  coef_table <- summary(m)$p.table
  
  data.frame(
    Model_ID = i,
    Variable = rownames(coef_table),
    Estimate = round(coef_table[, "Estimate"], 4),
    Std.Error = round(coef_table[, "Std. Error"], 4),
    z.value = round(coef_table[, "z value"], 4),
    p.value = round(coef_table[, "Pr(>|z|)"], 4)
  )
})

pval_df <- bind_rows(pval_list) %>%
  filter(!Variable %in% c("(Intercept)")) %>%
  arrange(Model_ID, p.value)

cat("✓ Extracted p-values from best models\n\n")
print(pval_df)

# ════════════════════════════════════════════════════════════════════════════
# EXPORT TO EXCEL
# ════════════════════════════════════════════════════════════════════════════

output_file <- "C:/Users/nassi/Desktop/Research/Acoustic monitoring reveals different responses of two endangered primates to primary forest degradation in the Ecuadorian Chocó/MKY_Reanalyses/Howler_Monkey_Model_Selection.xlsx"

wb <- createWorkbook()

# All models
addWorksheet(wb, "All_Models")
writeData(wb, "All_Models", as.data.frame(dredge_howler))
setColWidths(wb, "All_Models", cols = 1:min(15, ncol(dredge_howler)), widths = "auto")

# Best models
addWorksheet(wb, "Best_Models")
writeData(wb, "Best_Models", as.data.frame(best_howler))
setColWidths(wb, "Best_Models", cols = 1:min(15, ncol(best_howler)), widths = "auto")

# P-values and coefficients
addWorksheet(wb, "Best_Models_Pvalues")
writeData(wb, "Best_Models_Pvalues", pval_df)
setColWidths(wb, "Best_Models_Pvalues", cols = 1:6, widths = c(12, 25, 12, 12, 10, 10))

# High correlations
if (nrow(high_cor_pairs) > 0) {
  addWorksheet(wb, "Excluded_Pairs")
  writeData(wb, "Excluded_Pairs", high_cor_pairs)
  setColWidths(wb, "Excluded_Pairs", cols = 1:3, widths = c(25, 25, 15))
}

# Metadata
metadata <- data.frame(
  Item = c(
    "Sample size",
    "Occurrences (n)",
    "Occurrences (%)",
    "Number of predictors",
    "Global model AIC",
    "Global model deviance explained (%)",
    "Spatial smooth",
    "Max linear predictors per model",
    "Multicollinearity threshold",
    "Best models criterion"
  ),
  Value = c(
    nrow(df_howler),
    sum(df_howler$Occurrence),
    sprintf("%.1f", 100 * mean(df_howler$Occurrence)),
    length(available_vars),
    round(AIC(gam_howler_global), 2),
    round(100 * summary(gam_howler_global)$dev.expl, 2),
    "s(Latitude_scaled, Longitude_scaled, bs='tp', k=10)",
    "4",
    "|r| > 0.70",
    "ΔAICc ≤ 2"
  )
)

addWorksheet(wb, "Metadata")
writeData(wb, "Metadata", metadata)
setColWidths(wb, "Metadata", cols = 1:2, widths = c(35, 30))

saveWorkbook(wb, output_file, overwrite = TRUE)

cat(sprintf("\n✓ Results saved to:\n  %s\n\n", output_file))
cat("✓ Sheets created:\n")
cat("  - All_Models (all candidate models)\n")
cat("  - Best_Models (ΔAICc ≤ 2)\n")
cat("  - Best_Models_Pvalues (p-values and coefficients for each best model)\n")
if (nrow(high_cor_pairs) > 0) {
  cat("  - Excluded_Pairs (multicollinear pairs)\n")
}
cat("  - Metadata (analysis summary)\n\n")
cat("✓ Analysis complete!\n")

# ════════════════════════════════════════════════════════════════════════════
# MODEL AVERAGING
# ════════════════════════════════════════════════════════════════════════════

selected_models <- get.models(dredge_howler, subset = delta <= 2)
selected_weights <- best_howler$weight / sum(best_howler$weight)

cat(sprintf("✓ Averaging predictions from %d models\n\n", length(selected_models)))

# ════════════════════════════════════════════════════════════════════════════
# PARTIAL EFFECTS OF TOP PREDICTORS
# ════════════════════════════════════════════════════════════════════════════

# Extract top 4 predictors by frequency across best models
var_importance <- best_howler %>%
  as.data.frame() %>%
  select(-delta, -weight, -AIC, -df, -logLik) %>%
  select(where(~ !all(is.na(.)))) %>%
  summarise(across(everything(), ~ mean(!is.na(.)))) %>%
  tidyr::pivot_longer(everything(), names_to = "Variable", values_to = "Importance") %>%
  filter(!grepl("Intercept|Latitude_scaled|Longitude_scaled", Variable)) %>%
  arrange(desc(Importance)) %>%
  slice_head(n = 4)

top4_vars <- var_importance$Variable
cat("Top 4 predictors:\n")
print(var_importance)

# Variable name → display label + unit conversion
var_labels <- list(
  Elevation            = list(label = "Elevation"),
  Road_density_1000m   = list(label = "Road density\n(km/ha, 1000m)")
)

# Generate and print plots
plots <- lapply(names(var_labels), function(v) plot_partial_effect(v, df_howler))
for (p in plots) if (!is.null(p)) print(p)

# ════════════════════════════════════════════════════════════════════════════
# MODEL-AVERAGED HABITAT SUITABILITY PREDICTIONS
# ════════════════════════════════════════════════════════════════════════════

pred_matrix <- sapply(selected_models, function(m) {
  predict(m, newdata = df_grid, type = "response")
})

df_grid$HSM_howler_occurrence <- as.vector(pred_matrix %*% selected_weights)

# ── Habitat Suitability Map ─────────────────────────────────────────────────

df_grid_sf <- st_as_sf(df_grid, coords = c("Longitude", "Latitude"), crs = 4326)

ggplot(df_grid_sf) +
  geom_sf(aes(color = HSM_howler_occurrence), size = 0.65, shape = 15) +
  scale_color_gradientn(
    colours = palette_HSM, limits = c(0, 1),
    name = "Habitat suitability   "
  ) +
  coord_sf(expand = FALSE) +
  scale_x_continuous(
    breaks = seq(
      floor(min(st_coordinates(df_grid_sf)[, 1])),
      ceiling(max(st_coordinates(df_grid_sf)[, 1])),
      by = 0.09
    )
  ) +
  guides(
    color = guide_colorbar(
      barheight = unit(1.1, "cm"), barwidth = unit(15, "cm"),
      ticks = TRUE, frame.colour = "black", frame.linewidth = 1
    )
  ) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
    legend.key = element_rect(colour = "black", linewidth = 1),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 25),
    legend.position = "bottom"
  ) +
  labs(
    x = "Longitude", y = "Latitude"
  )

# ════════════════════════════════════════════════════════════════════════════
# SENSITIVITY TO INTACT FOREST COVER
# ════════════════════════════════════════════════════════════════════════════

library(ggplot2)
library(patchwork)

# Define interval labels
interval_labels <- c(
  "[0.00, 0.10]",
  "(0.10, 0.20]",
  "(0.20, 0.30]",
  "(0.30, 0.40]",
  "(0.40, 0.50]",
  "(0.50, 0.60]",
  "(0.60, 0.70]",
  "(0.70, 0.80]",
  "(0.80, 0.90]",
  "(0.90, 1.00]"
)

spider_plot <- df_grid %>%
  mutate(HSM_cat10 = cut(
    HSM_spider_occurrence,
    breaks = seq(0, 1, length.out = 11),
    include.lowest = TRUE,
    labels = interval_labels
  )) %>%
  ggplot(aes(x = Forest_undisturbed_500m, fill = HSM_cat10)) +
  geom_density(position = "fill", alpha = 0.9, linewidth = 2) +
  scale_fill_manual(values = palette_HSM) +
  coord_cartesian(expand = FALSE) + 
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
    legend.position = "none",
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 20)
  ) +
  labs(
    x = "Undisturbed forest cover (%, 500m)",
    y = "Proportion"
  )

howler_plot <- df_grid %>%
  mutate(HSM_cat10 = cut(
    HSM_howler_occurrence,
    breaks = seq(0, 1, length.out = 11),
    include.lowest = TRUE,
    labels = interval_labels
  )) %>%
  ggplot(aes(x = Forest_undisturbed_500m, fill = HSM_cat10)) +
  geom_density(position = "fill", alpha = 0.9, linewidth = 2) +
  scale_fill_manual(
    values = palette_HSM,
    name = "Habitat\nsuitability"
  ) +
  coord_cartesian(expand = FALSE) + 
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
    legend.position = "none",
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 20)
  ) +
  labs(
    x = "Undisturbed forest cover (%, 500m)",
    y = "Proportion"
  )

spider_plot + howler_plot +
  plot_layout(ncol = 2, widths = c(1, 1))

# ════════════════════════════════════════════════════════════════════════════
# MAP STUDY AREA
# ════════════════════════════════════════════════════════════════════════════

cat("\n========== LOADING AND MAPPING JRC_TMF DATA ==========\n\n")

# Load required libraries
library(tidyverse)
library(sf)
library(raster)
library(terra)
library(openxlsx)

cat("✓ Libraries loaded\n")

# ========== LOAD INPUT DATA ==========

# Load study plots from Excel
df_plots <- read.xlsx(
  "C:/Users/nassi/Desktop/Research/Acoustic monitoring reveals different responses of two endangered primates to primary forest degradation in the Ecuadorian Chocó/MKY_Reanalyses/MKY_data_reanalyses.xlsx",
  sheet = "sites description"
) %>%
  filter(!duplicated(Jocotoco_ID) | is.na(Jocotoco_ID)) %>%
  filter(!Jocotoco_ID %in% c(
    "G04_2025", "G22_2025", "G03_2025", "G13_2025",
    "GW62_2025", "G14_2025", "G05_2025", "G28_2025"
  ))

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

cat("✓ Study area defined\n")

# ========== LOAD AND CROP JRC_TMF RASTER ==========

# Load JRC_TMF raster
jrc_path <- "C:/Users/nassi/Downloads/JRC_TMF_TransitionMap_MainClasses_v1_1982_2025_SAM_ID47_N10_W80.tif"

jrc_raster <- terra::rast(jrc_path)
cat(sprintf("✓ Loaded JRC_TMF raster: %s\n", basename(jrc_path)))
cat(sprintf("  Dimensions: %d x %d\n", nrow(jrc_raster), ncol(jrc_raster)))
cat(sprintf("  CRS: %s\n", terra::crs(jrc_raster)))

# Crop to study area
jrc_cropped <- terra::crop(jrc_raster, terra::vect(study_area))
cat(sprintf("✓ Cropped to study area\n"))

# ========== CREATE MAP ==========

# Define JRC TMF Main Classes
jrc_classes <- data.frame(
  value = c(10, 20, 30, 41, 42, 43, 50, 60, 70),
  label = c(
    "Undisturbed tropical moist forest",
    "Degraded tropical moist forest",
    "Forest regrowth",
    "Deforested land (tree plantations)",
    "Deforested land (water)",
    "Deforested land (other land cover)",
    "Deforestation/degradation ongoing (2023-2025)",
    "Permanent and seasonal water",
    "Other land cover"
  ),
  color = c(
    "#1b7837",  # Dark green - Undisturbed
    "#91cf60",  # Light green - Degraded
    "#d9f0d3",  # Very light green - Regrowth
    "#fc9272",  # Orange - Plantations
    "#4575b4",  # Blue - Water
    "#f7f7f7",  # Light gray - Other
    "#e41a1c",  # Red - Recent disturbance
    "#a6cee3",  # Light blue - Water
    "#cccccc"   # Gray - Other land cover
  )
)

# Convert cropped raster to data frame for plotting with ggplot2
jrc_df <- as.data.frame(jrc_cropped, xy = TRUE) %>%
  as_tibble() %>%
  rename(value = 3) %>%  # Rename the raster value column
  left_join(jrc_classes, by = "value") %>%
  mutate(label = ifelse(is.na(label), "No Data", label)) %>%
  # Convert label to factor with custom order to preserve legend order
  mutate(label = factor(label, levels = c(
    "Undisturbed tropical moist forest",
    "Degraded tropical moist forest",
    "Forest regrowth",
    "Deforested land (tree plantations)",
    "Deforested land (water)",
    "Deforested land (other land cover)",
    "Deforestation/degradation ongoing (2023-2025)",
    "Permanent and seasonal water",
    "Other land cover",
    "No Data"
  )))

# Create the map
map <- ggplot() +
  # Plot raster
  geom_raster(data = jrc_df, aes(x = x, y = y, fill = label)) +
  
  # Plot study points
  geom_sf(data = sf_plots, aes(color = "Study sites"), size = 4, shape = 21, 
          fill = "white", stroke = 1.5) +
  
  # Styling
  scale_fill_manual(
    name = "JRC TMF Class",
    values = setNames(jrc_classes$color, jrc_classes$label),
    na.value = "#ffffff"
  ) +
  scale_color_manual(
    name = "",
    values = c("Study sites" = "black")
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 25),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 20)
  ) +
  coord_sf()

# Print map
print(map)

library(blockCV)
library(sf)
library(pROC)

# Convert to sf
sf_spider <- st_as_sf(
  df_data_spider_monkey %>% st_drop_geometry(),
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

sf_howler <- st_as_sf(
  df_data_howler_monkey %>% st_drop_geometry(),
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

# Create spatial blocks (k = 10, size = 10000 m)
blocks_spider <- cv_spatial(
  x      = sf_spider,
  column = "Occurrence",
  k      = 10,
  size   = 10000,
  seed   = 42,
  report = TRUE,
  plot   = TRUE
)

blocks_howler <- cv_spatial(
  x      = sf_howler,
  column = "Occurrence",
  k      = 10,
  size   = 10000,
  seed   = 42,
  report = TRUE,
  plot   = TRUE
)

# ════════════════════════════════════════════════════════════════════════════
# SPATIAL BLOCK CV FUNCTION
# ════════════════════════════════════════════════════════════════════════════

run_cv <- function(df_data, blocks, selected_models_list, model_weights, species_name) {
  
  cat(sprintf("\n--- %s ---\n\n", species_name))
  
  cv_results <- list()
  
  for (fold in seq_along(blocks$folds_list)) {
    
    train_idx <- blocks$folds_list[[fold]][[1]]
    test_idx  <- blocks$folds_list[[fold]][[2]]
    df_train  <- df_data[train_idx, ]
    df_test   <- df_data[test_idx, ]
    
    refitted_models  <- list()
    refitted_weights <- numeric()
    
    for (i in seq_along(selected_models_list)) {
      
      refitted_model <- tryCatch(
        gam(
          formula(selected_models_list[[i]]),
          data   = df_train,
          family = binomial(link = "logit"),
          select = TRUE
        ),
        error = function(e) {
          cat(sprintf("  Warning: model %d failed for fold %d\n", i, fold))
          NULL
        }
      )
      
      if (!is.null(refitted_model)) {
        refitted_models[[length(refitted_models) + 1]] <- refitted_model
        refitted_weights <- c(refitted_weights, model_weights[i])
      }
    }
    
    if (length(refitted_models) == 0) {
      cat(sprintf("  Error: no models refitted for fold %d — skipping\n", fold))
      next
    }
    
    refitted_weights <- refitted_weights / sum(refitted_weights)
    
    pred_matrix <- sapply(refitted_models, function(m)
      predict(m, newdata = df_test, type = "response")
    )
    
    pred_test <- if (length(refitted_models) == 1) {
      as.vector(pred_matrix)
    } else {
      as.vector(pred_matrix %*% refitted_weights)
    }
    
    cv_results[[fold]] <- data.frame(
      fold      = fold,
      observed  = df_test$Occurrence,
      predicted = pred_test
    )
  }
  
  cv_df <- bind_rows(cv_results)
  
  # AUC and Brier only
  roc_obj   <- roc(cv_df$observed, cv_df$predicted, quiet = TRUE)
  auc_val   <- as.numeric(roc_obj$auc)
  brier_val <- mean((cv_df$predicted - cv_df$observed)^2)
  
  cat(sprintf("AUC:         %.4f\n", auc_val))
  cat(sprintf("Brier score: %.4f\n\n", brier_val))
  
  list(
    species        = species_name,
    cv_predictions = cv_df,
    roc_object     = roc_obj,
    metrics        = data.frame(
      Species  = species_name,
      AUC      = round(auc_val,   4),
      Brier    = round(brier_val, 4),
      n_obs    = nrow(cv_df),
      n_blocks = length(blocks$folds_list)
    )
  )
}

# ════════════════════════════════════════════════════════════════════════════
# RUN FOR BOTH SPECIES
# ════════════════════════════════════════════════════════════════════════════

cv_spider <- run_cv(
  df_data              = df_data_spider_monkey %>% st_drop_geometry(),
  blocks               = blocks_spider,
  selected_models_list = get.models(dredge_spider, subset = delta <= 2),
  model_weights        = best_spider$weight / sum(best_spider$weight),
  species_name         = "Brown-headed Spider Monkey"
)

cv_howler <- run_cv(
  df_data              = df_data_howler_monkey %>% st_drop_geometry(),
  blocks               = blocks_howler,
  selected_models_list = get.models(dredge_howler, subset = delta <= 2),
  model_weights        = best_howler$weight / sum(best_howler$weight),
  species_name         = "Ecuadorian Mantled Howler Monkey"
)

# ════════════════════════════════════════════════════════════════════════════
# SUMMARY AND ROC CURVES
# ════════════════════════════════════════════════════════════════════════════

cat("\n========== SUMMARY ==========\n\n")
print(bind_rows(cv_spider$metrics, cv_howler$metrics))

par(mfrow = c(1, 2))
plot(cv_spider$roc_object, main = "Spider Monkey", col = "#1b7837", lwd = 3)
text(0.6, 0.2, sprintf("AUC = %.4f", cv_spider$metrics$AUC), col = "#1b7837", font = 2)
plot(cv_howler$roc_object, main = "Howler Monkey", col = "#e41a1c", lwd = 3)
text(0.6, 0.2, sprintf("AUC = %.4f", cv_howler$metrics$AUC), col = "#e41a1c", font = 2)
par(mfrow = c(1, 1))