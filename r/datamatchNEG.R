library(R.matlab)
library(tidyverse)

path <- "data/datamatchNEG.mat"
# m2r(path)


matdata <- readMat(path)
matdata1 <- matdata[[1]] #[1] and [[1]] are different

#subject info
##extract filename
filename1 <- matdata1$label[[1]]
filename <- filename1[[1]]
##extract samplename
meal1 <- matdata1$label[[5]]
meal <- meal1[[1]]
##extract protein source
protein_source1 <- matdata1$label[[9]]
protein_source <- protein_source1[[1]]
##extract gender info
gender1 <- matdata1$label[[13]]
gender <- gender1[[1]]

##extract time point
timepoint1 <- matdata1$class[[1]]
timepoint <- t(timepoint1[[1]])
##extract mz
mz1 <- matdata1$label[[6]]
mz <- mz1[[1]]
##extract rt
rt1 <- matdata1$label[[10]]
rt <- rt1[[1]]

##extract person
person1 <- matdata1$label[[21]]
person <- person1[[1]]
##extract intensities
int <- matdata1$data

##shape new data

data <- data.frame(filename,meal,protein_source,timepoint,person)
#  pools <- data %>% filter(samplename =='pool  ')
#  nonpools <- data %>% filter(samplename != 'pool  ')
#  pools$subject <- 'pool'
#  pools$timepoint <- NA
#  pools$intervention <- 'pool'
#  data <- bind_rows(nonpools,pools)

intdata <- data.frame(int)
datan <- bind_cols(data,intdata)
datan1 <- datan %>% gather(feature,intensity,6:ncol(datan))

##write rt and mz
rtdata <- t(rt) %>% data.frame() %>% gather(feature, rt, 1: nrow(rt))
mzdata <- t(mz) %>% data.frame() %>% gather(feature, mz, 1: nrow(mz))

datan2 <- left_join(datan1,rtdata,by='feature') %>% left_join(mzdata,by='feature')

datan3 <- datan2 %>% filter(meal==c('Barley','Pie   ','Soup  '))

features <- paste0('X',1:39)

excretionplot <- function (featurex){
  datan3 %>% group_by(feature,timepoint,meal) %>% 
    summarize(intmean=mean(intensity),intsd=sd(intensity)) %>% 
    mutate(intmean,intsd) %>% filter(feature==featurex & timepoint!=24) %>% 
    ggplot(aes(x=timepoint,y=intmean,group=meal,color=meal)) + 
    geom_line() + 
    geom_errorbar(aes(ymin=intmean-intsd,ymax=intmean+intsd)) +
    ggtitle(featurex)
}

excretionplotofprotein <- function (featurex){
  datan3 %>% group_by(feature,timepoint,protein_source) %>% 
    summarize(intmean=mean(intensity),intsd=sd(intensity)) %>% 
    mutate(intmean,intsd) %>% filter(feature==featurex & timepoint!=24) %>% 
    ggplot(aes(x=timepoint,y=intmean,group=protein_source,color=protein_source)) + 
    geom_line() + 
    geom_errorbar(aes(ymin=intmean-intsd,ymax=intmean+intsd)) +
    ggtitle(featurex)
}

excretionplotofprotein('X4')

lapply(features,excretionplot)

datan3 %>% group_by(feature,timepoint,person) %>% 
  summarize(intmean=mean(intensity),intsd=sd(intensity)) %>% 
  mutate(intmean,intsd) %>% filter(feature=='X1' & timepoint!=24) %>% 
  ggplot(aes(x=timepoint,y=intmean,group=person,color=person)) + 
  geom_line() + geom_errorbar(aes(ymin=intmean-intsd,ymax=intmean+intsd)) +
  ggtitle('X1')
