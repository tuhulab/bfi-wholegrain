library(readr)
library(readxl)
library(tidyverse)
library(astsa)
project <- "M226_barley"
sample <-"serum_plate_1"
mode <- "neg"
id <- paste(project,sample,mode,sep = "_")
data_extract <- function(id=...){
  peaklist <- file.path("XCMS_result",paste(id,"peaklist.csv",sep="_"))
  samplelist <- file.path("XCMS_result",paste(id,"samplelist.csv",sep="_"))
  dietcode <- "data/FullDietCodes.xlsx"
  data <- read_csv(peaklist) %>% 
    mutate(feature_index = paste0("Y",1:nrow(.)))
  intervention <- read_csv(samplelist) %>% mutate(sample=samplename) %>% left_join(
    readxl::read_excel(dietcode) %>% dplyr::select(sample,intervention)
  ) %>% mutate(intervention = ifelse(is.na(intervention), "Pool", intervention)) %>% dplyr::select(code, intervention)
  data_matrix <- list()
  data_matrix$X <- data %>% dplyr::select(X1:X61) %>% t
  data_matrix$Y <- read_csv(samplelist) %>% mutate(sample=samplename) %>% 
    left_join(readxl::read_excel(dietcode) %>% dplyr::select(sample,intervention)) %>% 
    mutate(intervention = ifelse(is.na(intervention), "Pool", intervention)) %>% dplyr::select(code, intervention)
  return(data_matrix)
}

data <- data_extract(id)

serum_pos_dirty <- read_csv("XCMS_result/M226_barley_serum_plate_1_pos_peaklist.csv")
serum_pos_sample <- read_csv("XCMS_result/M226_barley_serum_plate_1_pos_peaklist.csv")

serum_pos <- serum_pos_dirty %>% mutate(feature=paste0("y",1:nrow(.))) %>% 
  gather(key = "sample",value = "intensity",paste0("X",1:61)) %>% 
  left_join(data$Y %>% mutate(sample=code))

serum_pos_mvna <- serum_pos %>% mutate(intensity=ifelse(is.na(intensity),0,intensity))



t_test<- function(x){
  t_result <- t.test(x=serum_pos_mvna %>% filter(feature==paste0("y",x), intervention=="AW") %>% pull(intensity),
                     y=serum_pos_mvna %>% filter(feature==paste0("y",x), intervention=="AB") %>% pull(intensity))
  t_result$p.value}

n <- 1:dim(data$X)[2]
p_value <- sapply(n,t_test) 
test <- FDR(p_value)
which(p_value < .05) %>% length()
p.adjust(p_value,"fdr")




#####AR#####
ar_neutral_mass <- c(348.3028,
                     348.3028+28*1,
                     348.3028+28*2,
                     348.3028+28*3,
                     348.3028+28*4,
                     348.3028+28*5,
                     348.3028+28*6)
ar_mass_pos <- ar_neutral_mass + 1.007
ar_glu_pos <- ar_mass_pos + 176.06
(data$mz %in% ar_mass_pos )%>% sum
(data$mz %in% ar_glu_pos )%>% sum


mz_match <- function(mz=348.3028,data=...,window=0.015){
  n <- length(data$mz)
  
  
}