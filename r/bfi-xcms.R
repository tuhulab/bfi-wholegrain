library(tidyverse)
library(xcms)
library(readxl) #because 'readxl' is not a tidyverse core package, it should be loaded explicitly
#library(magrittr)
library(RColorBrewer)
library(pander)

#define cdf data location. (cdf data is converted by DataBridge, Waters (tm) )
folderpath<- 'C:/Users/tuhu/projects/bfi-wholegrain/data/cdf-urine'
cdffiles <- list.files(folderpath, recursive = TRUE)
cdffiles <- paste(folderpath,cdffiles,sep='/')

#test muyao's dataset
 muyao_path <- 'C:/Users/tuhu/projects/bfi-wholegrain/data/muyao_data'
 muyao_cdfs <- list.files(muyao_path, recursive = TRUE)
 muyao_cdfs <- paste(muyao_path,muyao_cdfs,sep='/')

#test serum dataset
serum_folder <- 'C:/Users/tuhu/projects/bfi-wholegrain/data/cdf-serum'
serum_cdffiles
 
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
 
  raw_data <- readMSData(files = pos_path, mode = "onDisk",msLevel. = 1, centroided. = TRUE,
                         pdata = pdata)
  
  ####test Muyao's data as a trail. It is ok!
  # raw_data <- readMSData(files = muyao_cdfs[1:30],mode = "onDisk",msLevel. = 1, centroided. = TRUE)
  
  #explore one peak
  head(rtime(raw_data)) #converted as second
  bpis <- chromatogram(raw_data, aggregationFun = "max")
  plot(bpis)
  
  
  ## Define the rt and m/z range of the peak area
  rtr <- c(250, 255)
  mzr <- c(291.20, 291.30)
  ## extract the chromatogram
  chr_raw <- chromatogram(raw_data, mz = mzr, rt = rtr)
  plot(chr_raw)
  
  ## Get the total ion current by file
  group_colors <- paste0(brewer.pal(3,'Set1')[1:2],'60')
  tc <- split (tic(raw_data), f= fromFile(raw_data))
  boxplot(tc, col=group_colors[raw_data$inervention],
          ylab= 'intensity', main= 'TIC')
  
#Define parameters for centWave algorithm (parameters were adapted from G?zde's noma method)
cwp <- CentWaveParam(ppm=30,
                     peakwidth= c(2,20)
                     #snthresh=4,
                     #prefilter=c(2,15),
                     #mzdiff=-0.001,
                     #integrate = 1,
                     #noise=15,mzCenterFun = 'mean'
                     )

cwp1 <- CentWaveParam(ppm=30,
                     peakwidth= c(2,20),
                     snthresh=4,
                     prefilter=c(2,15),
                     mzdiff=-0.001,
                     integrate = 1,
                     noise=15,mzCenterFun = 'mean')

register(SerialParam())
xdata <- findChromPeaks(raw_data, param = cwp)
xdata1 <- findChromPeaks(raw_data,param=cwp1)

head(chromPeaks(xdata1))

# detach('package:dplyr')
# detach('package:ggplot2')
# detach('package:purrr')
# detach('package:tidyr')
# detach('package:readxl')
# detach('package:readr')
# detach('package:tibble')
# detach('package:stringr')
# detach('package:tidyverse')

summary_fun <- function(z) {
  c(peak_count = nrow(z), rt = quantile(z[, "rtmax"] - z[, "rtmin"]))
}
T <- lapply(split.data.frame(chromPeaks(xdata),
                             f = chromPeaks(xdata)[, "sample"]),
            FUN = summary_fun)
T <- do.call(rbind, T)
rownames(T) <- basename(fileNames(xdata))
pandoc.table(T,
             caption = paste0("Summary statistics on identified chromatographic",
                              " peaks. Shown are number of identified peaks per",
                              " sample and widths/duration of chromatographic ",
                              "peaks."))

### summarize 
summary_fun <- function(z) {
  c(peak_count = nrow(z), rt = quantile(z[, "rtmax"] - z[, "rtmin"]))
}
T <- lapply(split.data.frame(chromPeaks(xdata1),
                             f = chromPeaks(xdata1)[, "sample"]),
            FUN = summary_fun)
T <- do.call(rbind, T)
rownames(T) <- basename(fileNames(xdata1))
pandoc.table(T,
             caption = paste0("Summary statistics on identified chromatographic",
                              " peaks. Shown are number of identified peaks per",
                              " sample and widths/duration of chromatographic ",
                              "peaks."))

## plot chrome peaks by each file
plotChromPeaks(xdata1,file=3, ylim = c(50,1000))

## alignment
xdata1_aligned <- adjustRtime(xdata1,param=ObiwarpParam(binSize = 0.6))
head(adjustedRtime(xdata1_aligned))
