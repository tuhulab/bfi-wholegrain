library(tidyverse)
library(readxl)
library(stringr)

wd <- getwd()
fullpath <- file.path(wd,'data','FullDietCodes.xlsx')
raw_data <- read_excel(fullpath)
samplename <- raw_data$sample
