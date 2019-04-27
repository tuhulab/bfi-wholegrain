.libPaths("C:/Program Files/R/R-3.5.3/library")

library(tidyverse)
library(xcms)
library(readxl)

#configure parallel computing
# registered()
# register(SerialParam()) 
###########configure parallel computing###########

register(SerialParam())
register(SnowParam(workers = 6))
##################################################

########access I drive##############
urine_sample_list_path<- "I:/SCIENCE-NEXS-NyMetabolomics/Personal folders/Tu/fatmed_cdf/urine_sample_list.xlsx"

#urine_sample_list_path <- file.path("C:","Users","czw814","projects","fatmed","data","urine_sample_list.xlsx")

read_MassLynx <- function(MassLynxSampleListPath=...){
  samplelist <- read_excel(MassLynxSampleListPath,col_names = c('filename',
                                                                'subject',
                                                                'MS_file',
                                                                'MS_Tune_file',
                                                                'Inlet_file',
                                                                'Bottle',
                                                                'Inject_volume'))
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

urine_sample_list <- read_MassLynx(urine_sample_list_path)


##Calculate running time of reading from portal harddrive (Toshiba)
urine_pos_pd_toshiba <- urine_sample_list %>% filter(polarity=="pos") %>% mutate(path = file.path("D:","FATMED_urine",paste0(.data$filename,"01.CDF")))
#start <- Sys.time()
raw_data_toshiba <- readMSData(files = urine_pos_pd_toshiba$path, 
                               mode = "onDisk",
                               msLevel. = 1, 
                               centroided. = TRUE)
#end <- Sys.time()
#time_from_network_raw_data <- end-start

##Calculate running time of reading from portal harddrive (Toshiba)
#start <- Sys.time()
mzs <- mz(raw_data_toshiba)
#end <- Sys.time()
#time_from_toshiba_mz <- end-start
#mzs <- mz(raw_data)
#start-end
mzs_by_file <- split(mzs, f=fromFile(raw_data))

#plot BPC 
bpis <- chromatogram(raw_data, aggregationFun="max")

#extract 1st chromatogram data
bpi_1 <- bpis[1,1]
head(rtime(bpi_1))
head(intensity(bpi_1))
length(intensity(bpi_1))

cwp <- CentWaveParam(ppm=30,
                      peakwidth= c(2,20),
                      snthresh=4,
                      prefilter=c(2,15),
                      mzdiff=-0.001,
                      integrate = 1,
                      noise=15,mzCenterFun = "mean")
xdata <- findChromPeaks(raw_data, param = cwp)

tc <- split(tic(raw_data), f = fromFile(raw_data))
boxplot(tc)

counterdown <- function(FUN=...){
  start <- Sys.time()
  FUN
  end <- Sys.time()
  return(end-start)}

test <- function(x) { round(sqrt(x), 4) }
counterdown(lapply(1:1000000,test
  ))

