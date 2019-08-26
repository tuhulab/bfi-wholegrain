library(tidyverse)
library(readxl)


libra_intra <- function(intensity=...,
                        elephant_frac=.05,
                        alpha=.1){
  require(tidyverse)
  data <- tibble(intensity_before_adjust = intensity,
                 injection_sequence = 1:length(intensity))
  fit_raw <- lm(intensity_before_adjust~injection_sequence,data) %>% summary()
  ifelse(fit_raw$coefficients[2,4]>alpha, return(data$intensity_before_adjust),{
    
    elephant <- tibble(index=which(fit_raw$residuals %in% (fit_raw$residuals %>% tibble() %>% top_frac(elephant_frac) %>% pull())),
                       intensity=data$intensity_before_adjust[index],
                       intensity_ant=index*fit_raw$coefficients[2] + fit_raw$coefficients[1])
    
    data_ant <- data %>% mutate(intensity_ant=intensity_before_adjust)
    data_ant$intensity_ant[elephant$index] <- elephant$intensity_ant
    
    fit_ant <- lm(intensity_ant~injection_sequence,data_ant) %>% summary()
    
    center <- length(data_ant$injection_sequence)/2 * fit_ant$coefficients[2] + fit_ant$coefficients[1]
    
    data_ant_cor <- data_ant %>% mutate(intensity_ant_adjust=fit_ant$residuals+center) %>%
      mutate(intensity_ant_adjust_elephant = intensity_ant_adjust)
    ###put back elephants
    data_ant_cor$intensity_ant_adjust_elephant[elephant$index] <- elephant$intensity
    ###put back sands
    data_ant_cor_sand<- data_ant_cor %>% mutate(intensity_ant_adjust_elephant_sand = ifelse(data_ant_cor$intensity_ant_adjust_elephant<0,
                                                                                            data_ant_cor$intensity_before_adjust,
                                                                                            data_ant_cor$intensity_ant_adjust_elephant))
    return(data_ant_cor_sand$intensity_ant_adjust_elephant_sand)})
}

#read raw data
raw_data <- read_csv("XCMS_result/M226_barley_serum_plate_1_neg_peaklist.csv")

#read one line of data and give it to libra_intra
t <- 1:nrow(raw_data)

## test
#intensity <- raw_data[500,7:ncol(raw_data)] %>% as.numeric() %>% ifelse(is.na(.),0.01,.)
##

adjust_data <- sapply(t,FUN = function(t){
  raw_data[t,7:ncol(raw_data)] %>% as.numeric() %>% ifelse(is.na(.),0.01,.)  %>% libra_intra(elephant_frac = .06,alpha = .15) 
})

pool_index <- suppressMessages(colnames(raw_data) %>% str_detect("Pool") %>% which)

pool_index <- pool_index - 6

intensity <- raw_data[3:nrow(raw_data),7:ncol(raw_data)]

###before libra###
pool_intensity <- intensity[,pool_index]
colnames(pool_intensity) <- colnames(pool_intensity) %>% str_replace(pattern = "\\...","_")

pool_intensity <- pool_intensity %>% mutate(feature=paste0("feature_",1:nrow(.))) %>% mutate_at(vars(-feature), as.numeric)
pool_intensity_summary <- pool_intensity %>% gather("sample","intensity",
                                                    colnames(pool_intensity)[colnames(pool_intensity) %>% str_detect("Pool")]) %>% 
  group_by(feature) %>% summarise(n = n(), mean = mean(intensity), sd = sd(intensity), cv_percent = sd/mean*100) %>% ungroup
pool_intensity_summary %>% ggplot(aes(cv_percent)) + geom_histogram()

###after libra###
# adjust_data[pool_index,]
sd <- colSds(adjust_data[pool_index,])
mean <- colMeans2(adjust_data[pool_index,])
cv_percent <- sd/mean *100
cv_percent_1 <- tibble(cv_percent)

cv_percent_1 %>% ggplot(aes(cv_percent)) + geom_histogram()


##
sum <- tibble(intensity = rowSums2(adjust_data[,1:1309]) ,
              injection_sequence = 1:length(intensity))
sum %>% ggplot(aes(injection_sequence,intensity)) + geom_point()
lm(intensity~injection_sequence,sum) %>% summary


sum <- tibble(intensity = rowSums2(adjust_data[,1310:2797]) ,
              injection_sequence = 1:length(intensity))
sum %>% ggplot(aes(injection_sequence,intensity)) + geom_point() + ggtitle("sum of intensity ~ injection sequence (urine neg)")
lm(intensity~injection_sequence,sum) %>% summary

write_csv(adjust_data %>% t() %>% as_tibble(),path = "libra_result/table1.csv")


###########test P value###########
raw_data <- read_excel("data/Fatmed_All_posneg.xlsx",1) %>% as_tibble()
libra_p_value <- function(intensity=...){
  require(tidyverse)
  data <- tibble(intensity_raw = intensity,
                 injection_sequence = 1:length(intensity))
  fit_raw <- lm(intensity_raw~injection_sequence,data) %>% summary()
  return(fit_raw$coefficients[2,4])}
t <- 3:nrow(raw_data)

p_value <- sapply(t,FUN = function(t){
  raw_data[t,7:ncol(raw_data)] %>% as.numeric() %>% libra_p_value() 
})


