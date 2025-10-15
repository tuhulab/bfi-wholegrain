# bfi-wholegrain

[![Codacy Badge](https://api.codacy.com/project/badge/Grade/82648908720d40129a4330fcaee1d795)](https://app.codacy.com/app/tu/bfi-wholegrain?utm_source=github.com&utm_medium=referral&utm_content=tuhulab/bfi-wholegrain&utm_campaign=Badge_Grade_Settings)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-%3E%3D3.6.0-blue.svg)](https://www.r-project.org/)
[![MATLAB](https://img.shields.io/badge/MATLAB-R2016a+-orange.svg)](https://www.mathworks.com/products/matlab.html)

Repository for Tu's master thesis project - **Investigation of Biomarkers for Whole Grain Cereal Intake**.

## Overview

This research compendium contains code and documentation for identifying and validating biomarkers of whole grain cereal intake using untargeted metabolomics (LC-MS) data. The project combines both R and MATLAB implementations for data preprocessing, statistical analysis, and biomarker discovery.

Most of the MATLAB code and algorithms were originally developed by supervisor Gözde Gürdeniz during her PhD and postdoc work at the University of Copenhagen. Tu adapted and extended these for his master thesis research.

## Repository Structure

```
.
├── r/                          # R scripts for data analysis
│   ├── preprocess_bfi_wholegrain.R  # Main preprocessing pipeline
│   ├── m2r.R                   # MATLAB to R data conversion
│   └── ...
├── matlab/                     # MATLAB scripts and functions
│   ├── Arrange_Data_MZmine2.m  # Data organization
│   ├── varselcv.m              # Variable selection with cross-validation
│   └── ...
├── data/                       # Raw and processed data (not tracked)
├── XCMS_result/                # XCMS processing results
├── presentation/               # Defense presentation materials
├── project-report/             # LaTeX source for project report
├── thesis-report/              # LaTeX source for thesis
└── README.md                   # This file
```

## Installation and Setup

### Prerequisites

- **R** (>= 3.6.0)
- **MATLAB** (>= R2016a) with the following toolboxes:
  - Statistics and Machine Learning Toolbox
  - Bioinformatics Toolbox

### R Dependencies

Install R package dependencies:

```r
# Install Bioconductor packages
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install(c("xcms", "CAMERA"))

# Install CRAN packages
install.packages(c(
  "RODBC", "kableExtra", "DT", "dplyr", "tidyverse",
  "readxl", "mixOmics", "R.matlab", "stringr", 
  "reshape2", "readr", "xlsx", "astsa"
))

# Install commonMZ (if available from source)
# devtools::install_github("source/commonMZ")
```

For reproducible dependency management, consider using `renv`:

```r
install.packages("renv")
renv::init()
renv::snapshot()
```

### Data Requirements

This project requires LC-MS metabolomics data in the following formats:
- **Waters raw data** (.raw format) for XCMS preprocessing
- **MZmine2 output** for alternative preprocessing
- **Sample metadata** in Excel format (.xlsx)

Place raw data files in the `data/` directory following the existing structure.

## Usage

### Data Preprocessing (R)

```r
# Source the main preprocessing script
source("r/preprocess_bfi_wholegrain.R")

# Or use the legacy version
source("preprocess.R")
```

### Data Analysis (MATLAB)

```matlab
% In MATLAB, navigate to the repository root and run:
cd matlab
arrange_serum_pos  % Process serum positive mode data
varselcv           % Perform variable selection with cross-validation
```

### MATLAB to R Conversion

```r
# Convert MATLAB data structures to R format
source("r/m2r.R")
```

## Key Analysis Steps

1. **Data Preprocessing**: XCMS-based peak detection and alignment
2. **Quality Control**: Blank subtraction and pool sample normalization
3. **Statistical Analysis**: PLS-DA, variable selection, cross-validation
4. **Biomarker Identification**: VIP scores, statistical significance testing
5. **Pathway Analysis**: Mummichog metabolic pathway enrichment

## Code Quality and Reproducibility

This project emphasizes:
- ✅ Explicit dependency management with version specifications
- ✅ Clear documentation of all analysis steps
- ✅ Modular, reusable functions
- ✅ Consistent coding style following tidyverse guidelines
- ✅ Version control with Git
- ✅ MIT license for open science

## Publications and Outputs

- **Master Thesis**: `thesis_tu.pdf`
- **Project Report**: Generated from `project-report/`
- **Defense Presentation**: `presentation/`

## R

The `r/` directory contains modular R scripts:
- `m2r.R`: Function to convert MATLAB datasets to R tidy data format
- `preprocess_bfi_wholegrain.R`: Complete preprocessing pipeline
- Additional analysis scripts for specific datasets

## Presentation

The `presentation/` folder stores the LaTeX source and PDF used during the thesis defense.

## Contributing

This is a completed thesis project, but suggestions and improvements are welcome. Please:
1. Fork the repository
2. Create a feature branch
3. Submit a pull request with clear description

## Citation

If you use this code or find it helpful, please cite:

```
Hu, T. (2017). Investigation of Biomarkers for Whole Grain Cereal Intake 
[Master's thesis, University of Copenhagen].
```

## Acknowledgements

Thank you to my supervisors **Lars Ove Dragsted** and **Gözde Gürdeniz** for their guidance throughout this research. Lars impressed me as an excellent teacher who can explain complex concepts in a vivid and understandable manner. Gözde provided detailed, supportive supervision at every step.

I acknowledge **Susanne Bügel** for providing the opportunity to analyze this data, and **Ashfaq Ali** for examining my thesis.

Special thanks to colleagues from the Metabolomics group: Jan, Giorgia, Xiaomin, Muyao, Natalia, Ceyda, Catalina, Sarah, Cecilie, Cristian, and Henrik. I especially appreciate my office-mate **Kristina** for sharing a wonderful year of metabolomics research, and **Nina Sondrup** for peer-reviewing my thesis.

Thanks also to researchers from the Section of Preventive and Clinical Nutrition (Steen, Inge, Simon, Kåre) and administrative staff (Claude, Geske, Krystyna, Randy) for their support.

Finally, thank you to my family, friends, and girlfriend for their constant encouragement despite the time zone differences.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
