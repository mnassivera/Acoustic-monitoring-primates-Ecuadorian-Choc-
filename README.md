# Acoustic-monitoring-primates-Ecuadorian-Choc-
Data from: Acoustic monitoring reveals contrasting responses of two endangered primates to primary forest degradation in the Ecuadorian Chocó

We applied passive acoustic monitoring (PAM) combined with a deep learning-based classification algorithm to detect vocalizations of two endangered primates—the Ecuadorian mantled howler monkey (Alouatta palliata aequatorialis) and the brown-headed spider monkey (Ateles fusciceps fusciceps)—across primary, secondary, and agricultural landscapes in the Canandé and nearby watersheds.

We selected 178 plots at the centre of which we deployed an Autonomous Recording Unit (ARU) : BAR-LT recorder (Frontier Labs) (175 plots) or a Song Meter Mini 2 Li-ion recorder (Wildlife Acoustics) (23 plots), equipped with a downward-facing omnidirectional microphone mounted approximately 1.70 m above the ground. Recorders captured 2-min files every 15 min throughout the day.

Training data included recordings from ARUs and field collections. The vocalizations were then segmented into 3-second clips to train the model with BirdNET Analyzer. To optimize model performance, we applied the built-in autotune function to search for the best set of hyperparameters (number of trials: 50, executions per trial: 1).

We applied this classifier to our entire recordings data. We set the model's confidence threshold to 0.99 to maximize precision, thereby minimizing false detections. Manual verification was conducted for all sites exhibiting ≤5 detections per species. This examination revealed elevated false positive rates for spider monkey classifications. Consequently, all spider monkey detections were manually verified to ensure accuracy. We thus included a column 'Check' in the dataset and filtered the data for future analyses.

See the Methods of the article for further details on the analyses.

Files and variables
File: Data_analysis.Rmd
Description: R Markdown document implementing the full data analysis pipeline for the study. It covers five main stages.

First, it imports and preprocesses the data.
Second, it provides a descriptive overview of the dataset, including summary statistics on detection counts and site-level occurrences for each species and a map of the study area overlaid with ARU sampling locations.
Third, it fits GAMs separately for the Brown-headed Spider Monkey and the Ecuadorian Mantled Howler Monkey. The scale of effect of landscape predictors is first evaluated by comparing buffer sizes. Backward variable selection is then performed, guided by AIC for occurrence models.
Fourth, for occurrence models, multi-model inference is applied: all models within ΔAICc ≤ 2 are retained and their predictions are averaged using Akaike weights. Model performance is evaluated through leave-one-out cross-validation (LOOCV), with AUC, Brier score, and classification accuracy as metrics.
Fifth, selected models are used to generate spatially continuous habitat suitability predictions across the prediction grid, which are then visualized as maps and summarized by habitat suitability class.
File: Extract_metrics_functions.R
Description: R script defining the functions used to extract and compute all landscape metrics reported in the plots_description and grid sheets. This includes the extraction of forest cover, distance to water and distance to forest edge from ESA WorldCover and JRC TMF2, the Forest Landscape Integrity Index (FLII), canopy height, NDVI, and road density.

File: Data.xlsx
Description: This dataset contains four sheets.

plots_description provides the characteristics of all field sites where an ARU was deployed. For each site, it includes identifiers (Plot_ID, Jocotoco_ID, Soundbox_ID), geographic information (longitude, latitude, elevation), and a suite of landscape metrics: Forest Landscape Integrity Index (FLII) from Grantham et al. at 500 m and 2000 m buffers; forest cover derived from ESA WorldCover, JRC TMF2, and a combination of both products at 500 m and 2000 m buffers; canopy height from Lang et al. and Tolan et al. at 100 m buffers; NDVI from Google Earth Engine at 500 m and 2000 m buffers; distance to water and to forest edge (using both ESA and JRC basemaps); and road density from the GRIP dataset at 500 m and 2000 m buffers. For Reassembly plots, additional attributes describe the surrounding matrix, plot category, and regeneration year for secondary forest sites.
grid provides the same landscape metrics restricted to those retained by the Generalized Additive Models (GAMs) for each pixel of the prediction grid, enabling spatial extrapolation of model outputs across the study area.
observation contains all detections of spider monkey and howler monkey vocalizations identified by the BirdNET Analyzer classifier across all ARU recordings. Each row corresponds to a single detection and includes the start and end time within the audio file, species scientific and common name, classifier confidence score, the source audio file name, and a manual validation flag (Check): detections marked 'x' were manually reviewed and confirmed as false positives.
observation_filtered is a curated version of the observation sheet, retaining only detections that passed manual validation (i.e., excluding all rows where Check == 'x'). This is the version used in all subsequent analyses.
Code/software
R version: 4.2.3

