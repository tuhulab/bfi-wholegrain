library(readr)
library(readxl)
library(tidyverse)

serum_pos_dirty <- read_csv("data/serum_pos_feature.csv")

deisotope <- function(data=...){
  require(stringr)
  str_which(data$isotopes, pattern = "\\+\\d")
  data_1 <- data[-str_which(data$isotopes, pattern = "\\+\\d|\\d\\+"),]
  return(data_1)}

serum_pos <- deisotope(serum_pos_dirty) %>% mutate(feature=paste0("y",1:nrow(.))) %>% 
  select(-isotopes) %>% 
  gather(key = "sample",value = "intensity",paste0("X",1:61)) %>% 
  left_join(tibble(sample=paste0("X",1:61),intervention=spl_mode$sample_group))

serum_pos_mvna <- serum_pos %>% mutate(intensity=ifelse(is.na(intensity),0,intensity))



t_test<- function(x){
  t_result <- t.test(x=serum_pos_mvna %>% filter(feature==paste0("y",x), intervention=="AW") %>% pull(intensity),
                     y=serum_pos_mvna %>% filter(feature==paste0("y",x), intervention=="AB") %>% pull(intensity))
  t_result$p.value}

n <- 1:1064
p_value <- sapply(n,t_test) 
which(p_value < .05)
serum_pos_dirty %>% mutate(p_value)
