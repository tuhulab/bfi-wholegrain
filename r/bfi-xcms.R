library(xcms)
library(tidyverse)
library(readxl) #because 'readxl' is not a tidyverse core package, it should be loaded explicitly
#library(magrittr)

#define cdf data location. (Here, cdf data is converted by DataBridge, Waters (tm) )
folderpath<- 'C:/Users/tuhu/projects/bfi-wholegrain/data/trail'
cdffiles <- list.files(folderpath, recursive = TRUE)
cdffiles <- paste(folderpath,cdffiles,sep='/')

#create a phenodata data.frame (store subjects, filename, intervention, etc)
#for KU_nexs_metabolomics group, it will be a MassLynx samplelist plus a diet_code
#samplelist_path <- 'C://Users//tuhu//projects//bfi-wholegrain//matlab//urine_samplelist.xlsx'

#temporary solution
pd <- data.frame(sample_name = sub(basename(cdffiles), pattern = ".CDF",
                                   replacement = "", fixed = TRUE),
                 # sample_group = c(rep("fake_gr_1", 4), rep("fake_gr_2", 3)),
                 stringsAsFactors = FALSE)

raw_data <- readMSData(files = cdffiles, pdata = new("NAnnotatedDataFrame", pd),
                       mode = "onDisk")

#next line is only for testing purpose (NOT FINISH, I'm gonna do this later)
# MassLynxSampleListPath <- samplelist_path
# MassLynx_extract <- function(MassLynxSampleListPath=...){
#   samplelist <- read_excel(MassLynxSampleListPath,col_names = FALSE)
#   filename <- samplelist[,1]
#   subject <- samplelist[,2]
#   polarity <- samplelist[,3]
#     extract_polarity <- function(polarity){
#       polarity1 <- polarity
#         lapply  
#         str_match(as.character(polarity1[1,]),'pos')
#     } 
# } 


#Define parameters for centWave algorithm (parameters were adapted from Gözde's noma method)
cwp <- CentWaveParam(ppm=35,
                     peakwidth= c(2,20),
                     snthresh=4,
                     prefilter=c(2,15),
                     mzdiff=-0.001,
                     integrate = 1)

xdata <- findChromPeaks(raw_data, param = cwp)

                      
