library(tidyverse)
library(readr)
library(readxl)
####read data####
peaklist <- read_delim("XCMS_result/M226_barley_urine_plate_1_pos_peaklist.csv",";")
samplelist <- read_csv("XCMS_result/M226_barley_urine_plate_1_pos_samplelist.csv") %>% 
  left_join(read_excel("gender_info.xlsx") 
            %>% mutate(subject=as.character(subject)))

peaklist %>% filter(mz>291.22,
                    mz<291.24) %>% 

