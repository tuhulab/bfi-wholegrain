.libPaths("C:/Program Files/R/R-3.5.3/library")

library(tidyverse)
library(xcms)
library(readxl)

urine_sample_list_path <- file.path("C:","Users","czw814","projects","fatmed","data","urine_sample_list.xlsx")

read_MassLynx <- function(MassLynxSampleListPath=...){
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

urine_sample_list <- read_MassLynx(urine_sample_list_path)
urine_pos_pd <- urine_sample_list %>% filter(polarity=="pos") %>% mutate(path = file.path("C:","Users","czw814","projects","fatmed","data","FATMED_urine",paste0(.data$filename,"01.CDF")))

raw_data <- readMSData(files = urine_pos_pd$path, mode = "onDisk",msLevel. = 1, centroided. = TRUE)
