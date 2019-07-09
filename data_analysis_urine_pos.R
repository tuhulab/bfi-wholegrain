library(tidyverse)
library(xlsx)
library(readr)
library(readxl)
library(mixOmics)

diet_code <- read_excel("matlab/DietCodes.xlsx")
sample_list <- read_csv("XCMS_result/M226_barley_urine_plate_1_pos_samplelist.csv") %>% left_join(diet_code)

urine_pos <- read_delim("XCMS_result/M226_barley_urine_plate_1_pos_peaklist.csv",delim = ";") %>% mutate(feature=paste0("y",1:nrow(.))) %>% 
     gather(key = "code",value = "intensity",paste0("X",1:61)) %>% 
     left_join(sample_list)

urine_pos_mvna <- urine_pos %>% mutate(intensity=ifelse(is.na(intensity),0,intensity))

t_test<- function(x){
  t_result <- t.test(x=urine_pos %>% filter(feature==paste0("y",x), intervention=="AW") %>% pull(intensity),
                     y=urine_pos %>% filter(feature==paste0("y",x), intervention=="AB") %>% pull(intensity))
  t_result$p.value}

n <- 312:312
p_value <- sapply(n,t_test) 
which(p_value < .05)
serum_pos_dirty %>% mutate(p_value)


# serum_pos <- deisotope(serum_pos_dirty) %>% mutate(feature=paste0("y",1:nrow(.))) %>% 
#   select(-isotopes) %>% 
#   gather(key = "sample",value = "intensity",paste0("X",1:61)) %>% 
#   left_join(tibble(sample=paste0("X",1:61),intervention=spl_mode$sample_group))
# 
urine_pos %>% filter(feature=="y290") %>% ggplot(aes(intervention,intensity)) + geom_point() + geom_boxplot()
 
# urine %>% filter(mz<485,
#                 mz>484,
#                 rt>4,
#                 rt<5) %>% gather(code,intensity,paste0("X",1:61)) %>% left_join(sample_list)%>% ggplot(aes(intervention,intensity)) + geom_point() + geom_boxplot()

 barleypos %>% filter(mz<485,
                 mz>484,
                 rt>4,
                 rt<5) %>% gather(code,intensity,paste0("X",1:61)) %>% left_join(sample_list)%>% ggplot(aes(intervention,intensity)) + geom_point() + geom_boxplot()

