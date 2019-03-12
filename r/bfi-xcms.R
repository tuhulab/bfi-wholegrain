library(xcms)
library(tidyverse)
library(readxl) #because 'readxl' is not a tidyverse core package, it should be loaded explicitly
#library(magrittr)
library(RColorBrewer)

#define cdf data location. (Here, cdf data is converted by DataBridge, Waters (tm) )
folderpath<- 'C:/Users/tuhu/projects/bfi-wholegrain/data/cdf-urine'
cdffiles <- list.files(folderpath, recursive = TRUE)
cdffiles <- paste(folderpath,cdffiles,sep='/')

#create a phenodata data.frame (store subjects, filename, intervention, etc)
#for KU_nexs_metabolomics group, it will be a MassLynx samplelist plus a diet_code
samplelist_path <- 'C://Users//tuhu//projects//bfi-wholegrain//matlab//urine_samplelist.xlsx'

MassLynxSampleListPath <- samplelist_path
MassLynx_extract <- function(MassLynxSampleListPath=...){
  samplelist <- read_excel(MassLynxSampleListPath,col_names = c('filename','subject','MS_file','MS_Tune_file','Inlet_file','Bottle','Inject_volume'))
  filename <- samplelist[,1]
  subject <- samplelist[,2]
  polarity <- samplelist[,3]
    #use character match to determine polarity
    extract_pos <- function(rownumber=...){
      polarity1 <- polarity
      pos_or_not <- grepl('pos',polarity[rownumber,])
      return(pos_or_not)
    }
  #generate a list
  polarity_T_or_F <- sapply(1:nrow(polarity), extract_pos)
  polarity <- ifelse(polarity_T_or_F,'pos','neg')
  pd <- data.frame(filename,subject,polarity=polarity)
  return(pd)
}
pd_1 <- MassLynx_extract(MassLynxSampleListPath)

#diet codes
diet_code_path <- 'C:/Users/tuhu/projects/barley/DietCodes.xlsx'
diet_code_1 <- read_excel(diet_code_path) %>% as.tibble()
diet_code <- diet_code_1 %>% mutate(subject=sample,intervention=intervention) %>% 
  select(subject,intervention)
pd_2 <- left_join(pd_1,diet_code,by='subject')
pd_samples <- pd_2 %>% filter(is.na(intervention)==FALSE)
pd_non_samples <- pd_2 %>% filter(is.na(intervention)==TRUE) %>% mutate(intervention=subject)
pd <- bind_rows(pd_samples,pd_non_samples)

##select only POS mode (samples+pool)
  pd_pos <- pd %>% filter(polarity=='pos',intervention %in% c('BW','BB','AW','AB','Pool'))
  pos_path <- paste(folderpath,paste0(pd_pos$filename,'01.CDF'),sep='/')
  
  pdata <- new('NAnnotatedDataFrame', pd_pos)
 
  raw_data <- readMSData(files = pos_path,mode = "onDisk",pdata = pdata)

  #explore one peak
  ## Define the rt and m/z range of the peak area
  rtr <- c(250, 300)
  mzr <- c(291.26, 291.28)
  ## extract the chromatogram
  chr_raw <- chromatogram(raw_data, mz = mzr, rt = rtr)
  plot(chr_raw)
  
  
#Define parameters for centWave algorithm (parameters were adapted from Gözde's noma method)
cwp <- CentWaveParam(ppm=35,
                     peakwidth= c(2,20),
                     snthresh=4,
                     prefilter=c(2,15),
                     mzdiff=-0.001,
                     integrate = 1,
                     noise=5000,mzCenterFun = 'mean')

xdata <- findChromPeaks(raw_data, param = cwp)

cwp <- CentWaveParam(peakwidth = c(20, 80), noise = 5000)
xdata <- findChromPeaks(raw_data, param = cwp)

                      
