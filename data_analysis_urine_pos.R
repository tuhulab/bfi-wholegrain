library(tidyverse)
library(xlsx)
library(readr)
library(readxl)
library(mixOmics)

diet_code <- read_excel("matlab/DietCodes.xlsx") %>% rename(samplename=sample)
sample_list <- read_csv("XCMS_result/M226_barley_urine_plate_1_pos_samplelist.csv") %>% left_join(diet_code)

urine_pos <- read_csv("XCMS_result/M226_barley_urine_plate_1_pos_peaklist.csv") %>% 
   mutate(feature=paste0("y",1:nrow(.))) %>% 
     gather(key = "code",value = "intensity",paste0("X",1:61)) %>% 
     left_join(sample_list) %>% mutate(subject=samplename %>% str_extract("\\d{4}|Pool"))

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
urine_pos %>% filter(mz<554, mz>553) %>% ggplot(aes(intervention,intensity)) + geom_point() + geom_boxplot()
 
# urine %>% filter(mz<485,
#                 mz>484,
#                 rt>4,
#                 rt<5) %>% gather(code,intensity,paste0("X",1:61)) %>% left_join(sample_list)%>% ggplot(aes(intervention,intensity)) + geom_point() + geom_boxplot()




 
 #####sex_hormon###
 urine_pos %>% filter(feature=="y842") %>% 
   left_join(read_excel("gender_info.xlsx") %>% 
               mutate(subject=as.character(subject))) %>% 
   mutate(Gender=ifelse(is.na(gender),"pool",gender),
          Intensity=intensity) %>%
   ggplot(aes(Gender,Intensity)) + geom_point() + geom_boxplot() + ggtitle("Intensities of Androsterone in Different Genders")
 
 urine_pos %>% filter(feature=="y842") %>% 
   left_join(read_excel("gender_info.xlsx") %>% 
               mutate(subject=as.character(subject))) %>% 
   mutate(Gender=ifelse(is.na(gender),"pool",gender),
          Intensity=intensity)  %>%
   ggplot(aes(intervention,Intensity)) + geom_point() + geom_boxplot() + ggtitle("Intensities of Androsterone in Different Genders")

 
 andro_gender <-  urine_pos %>% filter(feature=="y842") %>% 
    left_join(read_excel("gender_info.xlsx") %>% 
                 mutate(subject=as.character(subject))) %>% 
    mutate(Gender=ifelse(is.na(gender),"pool",gender),
           Intensity=intensity)
andro_male <- andro_gender %>% filter(Gender=="M")
andro_female <- andro_gender %>% filter(Gender=="F")

andro_male$intensity %>% mean/andro_female$intensity %>% mean
andro_female$intensity %>% mean
