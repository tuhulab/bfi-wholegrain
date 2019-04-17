install.packages("devtools")
install.packages("tidyverse")

if (!requireNamespace("BiocManager"))
  install.packages("BiocManager")
BiocManager::install()

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("xcms", version = "3.8")

.libPaths("C:/Program Files/R/R-3.5.3/library")
library(tidyverse)

