library(tidyverse)
library(readxl)

feature_table <- read_delim("XCMS_result/M226_barley_urine_plate_1_neg_peaklist.csv",";") %>% 
  gather(code,intensity,paste0("X",1:61)) %>% left_join(read_csv("XCMS_result/M226_barley_urine_plate_1_neg_samplelist.csv")) %>% 
  left_join(read_excel("matlab/DietCodes.xlsx"))

feature_table %>% filter(mz<343.1,mz>343.05, rt_min<1.80, rt_min>1.50) %>% ggplot(aes(intervention,intensity)) + geom_point() + geom_boxplot()

feature_table %>% filter(mz<343.1,mz>343.05, rt_min<2.4, rt_min>2) %>% ggplot(aes(intervention,intensity)) + geom_point() + geom_boxplot()


feature_table %>% filter(mz<286,mz>285, rt_min<2.7, rt_min>2) %>% ggplot(aes(intervention,intensity)) + geom_point() + geom_boxplot()
feature_table %>% filter(mz<286,mz>285, rt_min<1, rt_min>0) %>% ggplot(aes(intervention,intensity)) + geom_point() + geom_boxplot()


feature_table %>% filter(mz<372,mz>371, rt_min<3.4, rt_min>3) %>% ggplot(aes(intervention,intensity)) + geom_point() + geom_boxplot()
feature_table %>% filter(mz<372,mz>371, rt_min<1.1, rt_min>0) %>% ggplot(aes(intervention,intensity)) + geom_point() + geom_boxplot()

