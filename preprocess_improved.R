#' Metabolomics Data Preprocessing Pipeline
#'
#' This script performs LC-MS data preprocessing using XCMS and CAMERA packages.
#' It includes peak detection, alignment, grouping, and annotation steps.
#'
#' @author Tu Hu
#' @date 2017
#' @license MIT

# Load Required Packages --------------------------------------------------

library(RODBC)
library(kableExtra)
library(DT)
library(dplyr)
library(CAMERA)
library(commonMZ)
library(xcms)
library(tidyverse)
library(readxl)
library(parallel)

# Configuration -----------------------------------------------------------

#' Project Configuration
#' @description Set project-specific parameters
project_name <- "M226_barley"    # Project identifier (uppercase)
analysis_name <- "urine_plate_1" # Analysis batch (lowercase)
mode <- "neg"                     # MS polarity: "pos" or "neg"

# Generate identifiers
id <- paste(project_name, analysis_name, mode, sep = "_")
id_1 <- paste(project_name, analysis_name, sep = "_")
mzML_dir <- "data/BARLEY_urine_raw.pro/Data/"

# Helper Functions --------------------------------------------------------

#' Extract Sample List from Waters SPL File
#'
#' Connects to a Waters sample list (.SPL) file and extracts sample metadata.
#'
#' @param file Character string, path to .SPL file
#' @return A tibble with sample metadata
#' @export
waters_extract_spl <- function(file) {
  require(RODBC)
  
  # Connect to Access database
  con <- odbcConnectAccess2007(file)
  if (con == -1) {
    warning("Failed to connect to SPL file: ", file)
    return(tibble())  # Updated from deprecated data_frame()
  }
  
  # Fetch analysis table and clean data
  out <- sqlFetch(con, "ANALYSIS", stringsAsFactors = FALSE) %>%
    mutate(across(one_of('SAMPLE_LOCATION', 'FILE_NAME'), as.character)) %>%
    mutate(across(contains('CONC_'), as.character))
  
  close(con)
  return(out)
}

# Data Loading ------------------------------------------------------------

message("Loading sample list from SPL file...")

spl <- waters_extract_spl(file.path("spl", paste0(id_1, ".SPL"))) %>%
  select(samplename = FILE_TEXT, 
         filename = FILE_NAME, 
         polarity = MS_FILE) %>% 
  as_tibble()

# Process polarity information
polarity <- spl$polarity
polarity[str_which(polarity, "pos")] <- "pos"
polarity_1 <- ifelse(polarity == "pos", "pos", "neg")

# Filter samples
spl_1 <- spl %>% 
  mutate(polarity = polarity_1) %>% 
  filter(samplename != "")

spl_mode <- spl_1 %>% 
  filter(
    polarity == mode,
    !samplename %in% c("Blank", "Blank+IntStd", "MetStd", "MetStd+IntStd")
  ) %>% 
  mutate(sample_group = "one")

# Verify file existence
mzml_files <- file.path(mzML_dir, paste0(spl_mode$filename, ".mzML"))
files_exist <- file.exists(mzml_files)

if (!all(files_exist)) {
  warning("Missing mzML files: ", sum(!files_exist), " out of ", length(files_exist))
}

message("Found ", sum(files_exist), " mzML files")

# XCMS Parameter Configuration --------------------------------------------

message("Configuring XCMS parameters...")

params <- list()

# Peak detection parameters (CentWave)
params$CentWave <- CentWaveParam(
  ppm = 30,
  peakwidth = c(0.025 * 60, 0.30 * 60),  # 1.5 to 18 seconds
  snthresh = 10,
  noise = 0,
  prefilter = c(3, 30),
  integrate = 2,
  mzdiff = -0.001,
  verboseColumns = TRUE,
  fitgauss = TRUE
)

# First grouping parameters
params$group1 <- PeakDensityParam(
  sampleGroups = spl_mode$sample_group, 
  binSize = 0.01,        # m/z bin size in Da
  bw = 0.2 * 60,         # Retention time bandwidth
  minSamples = 1,
  minFraction = 0.15,
  maxFeatures = 10
)

# Retention time alignment parameters
params$PeakAlign <- PeakGroupsParam(
  smooth = "loess",
  span = 0.6,
  minFraction = 0.9,
  family = "gaussian",
  extraPeaks = 3
)

# Second alignment (same params)
params$PeakAlign2 <- params$PeakAlign

# Annotation parameters
mz_window <- 0.015  # m/z tolerance for annotation
rt_window <- 10     # RT tolerance in seconds

# Data Processing ---------------------------------------------------------

message("Step 1: Reading raw MS data...")
raw <- readMSData(
  mzml_files, 
  pdata = new("NAnnotatedDataFrame", spl_mode), 
  mode = "onDisk", 
  msLevel. = 1
)

message("Step 2: Peak detection (CentWave)...")
xset <- findChromPeaks(raw, param = params$CentWave)

message("Step 3: First peak grouping...")
xset_g <- groupChromPeaks(xset, param = params$group1)

message("Step 4: Retention time alignment...")
xset_g_r <- adjustRtime(xset_g, param = params$PeakAlign)

# Visualization of RT alignment
plotAdjustedRtime(
  xset_g_r, 
  col = as.factor(pull(pData(xset_g_r), sample_group)), 
  peakGroupsCol = "grey", 
  peakGroupsPch = 1
)

message("Step 5: Second peak grouping (refined)...")
params$group2 <- PeakDensityParam(
  sampleGroups = xset_g_r %>% pData() %>% pull(sample_group), 
  binSize = 0.1, 
  bw = 0.1 * 60,
  minSamples = 1,
  minFraction = 0.20,
  maxFeatures = 5
)

xset_g_r_g <- groupChromPeaks(xset_g_r, param = params$group2)

message("Step 6: Second retention time alignment...")
xset_g_r_g_r <- adjustRtime(
  applyAdjustedRtime(xset_g_r_g), 
  param = params$PeakAlign2
)

plotAdjustedRtime(
  xset_g_r_g_r, 
  col = as.factor(pull(pData(xset_g_r_g_r), sample_group)), 
  peakGroupsCol = "grey", 
  peakGroupsPch = 1
)

message("Step 7: Peak filling...")
params$FillChromPeaks <- FillChromPeaksParam(
  expandMz = 0, 
  expandRt = 0, 
  ppm = 30
)

xset_g_r_g_fill <- fillChromPeaks(
  xset_g_r_g, 
  params$FillChromPeaks, 
  BPPARAM = SnowParam(min(4, detectCores() - 1))
)

# Extract Results ---------------------------------------------------------

message("Extracting peak intensities...")
peaks_aligned <- featureValues(
  xset_g_r_g_fill, 
  value = "into",
  method = "medret",
  intensity = "into", 
  filled = TRUE,
  missing = NA
)

# Display preview
kable(peaks_aligned[1:5, ], format = "html") %>% 
  kable_styling(font_size = 9)

feature_def <- featureDefinitions(xset_g_r_g_fill)

datatable(as_tibble(feature_def)) %>% 
  formatRound(columns = c("mzmed", "mzmin", "mzmax"), digits = 4) %>% 
  formatRound(columns = c("rtmed", "rtmin", "rtmax"), digits = 2)

# CAMERA Annotation -------------------------------------------------------

message("Step 8: CAMERA annotation - isotopes and adducts...")

# Determine polarity for CAMERA
camera_polarity <- ifelse(mode == "pos", "positive", "negative")

xsa <- xsAnnotate(
  as(xset_g_r_g_fill, "xcmsSet"), 
  sample = NA, 
  nSlaves = max(1, detectCores() - 1), 
  polarity = camera_polarity
)

xsaF <- groupFWHM(xsa, perfwhm = 0.1, intval = "into", sigma = 6)

xsaC <- groupCorr(
  xsaF,
  calcIso = FALSE, 
  calcCiS = TRUE, 
  calcCaS = TRUE, 
  cor_eic_th = 0.7,
  cor_exp_th = 0.7,
  pval = 0.000001, 
  graphMethod = "lpc",
  intval = "into"
)

xsaFI <- findIsotopes(xsaC, ppm = 10, mzabs = 0.01, intval = "into")

# Load adduct rules
rules <- MZ_CAMERA(mode = mode, warn_clash = TRUE, clash_ppm = 5)
rules <- as.data.frame(rules)

# Find adducts
xsaFA <- findAdducts(
  xsaFI, 
  ppm = 10, 
  mzabs = 0.01, 
  multiplier = 4, 
  polarity = mode, 
  rules = rules
)

peaklist <- getPeaklist(xsaFA)

# Display annotation results
peaklist %>% 
  select("mz", "rt", "isotopes", "adduct", "pcgroup") %>% 
  kable(format = "html", padding = 0) %>% 
  kable_styling(font_size = 10)

# Data Export -------------------------------------------------------------

message("Step 9: Preparing output...")

pos_peaklist <- peaklist %>% 
  as_tibble() %>% 
  select(-one)

# Round values for export
data_output <- data.frame(
  mz = round(pos_peaklist$mz, 4),
  mzmin = round(pos_peaklist$mzmin, 4),
  mzmax = round(pos_peaklist$mzmax, 4),
  rt = round(pos_peaklist$rt / 60, 2),
  rtmin = round(pos_peaklist$rtmin / 60, 2),
  rtmax = round(pos_peaklist$rtmax / 60, 2)
) %>% 
  bind_cols(pos_peaklist[, 7:ncol(pos_peaklist)]) %>% 
  select(-npeaks) %>% 
  as_tibble()

# Deisotoping
message("Removing isotope peaks...")
data_output_deisotope <- data_output %>% deisotope()

# Annotation with database
message("Annotating against KUDB...")
data_output_annotation <- annotate_kudb(
  mz_window,
  rt_window / 60,
  polarity = mode,
  data = data_output_deisotope
)

# Create output directory if needed
output_dir <- "xcms_result"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Write results
message("Writing output files...")
write_csv(
  data_output_annotation,
  path = file.path(output_dir, paste(id, "peaklist.csv", sep = "_"))
)

write_csv(
  spl_mode %>% 
    select(-sample_group) %>% 
    mutate(code = paste0("X", seq_len(n()))),
  path = file.path(output_dir, paste(id, "samplelist.csv", sep = "_"))
)

message("Processing complete!")
message("Output files saved to: ", output_dir)
