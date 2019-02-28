library(tidyverse)
library(R.matlab)

path <- 'c://Users/tuhu/projects/barley/urine_pos_varsel.mat'
polarity <- 'pos'

m2r <- function(path=...,polarity=...){
  matdata <- readMat(path)
  matdata1 <- matdata[[1]] #[1] and [[1]] are different
  
  #subject info
  ##extract filename
  filename1 <- matdata1$label[[1]]
  filename <- filename1[[1]]
  ##extract samplename
  samplename1 <- matdata1$label[[5]]
  samplename <- samplename1[[1]]
  ##extract subjectnumber
  subject1 <- matdata1$label[[13]]
  subject <- subject1[[1]]
  ##extract timepoint
  timepoint1 <- matdata1$label[[17]]
  timepoint <- timepoint1[[1]]
  ##extract intervention
  intervention1 <- matdata1$label[[21]]
  intervention <- intervention1[[1]]
  ##extract rt
  rt1 <- matdata1$axisscale[[6]]
  rt <- rt1[[1]]
  ##extract mz
  mz1 <- matdata1$label[[2]]
  mz <- mz1[[1]]
  ##extract groupinfo
  #group1 <- matdata1$label[[10]]
  #group <- group1[[1]]
  ##extract intensities
  int <- matdata1$data
  
  ##shape new data
  
  data <- data.frame(filename,samplename,subject,timepoint,intervention)
  pools <- data %>% filter(samplename =='pool  ')
  nonpools <- data %>% filter(samplename != 'pool  ')
  pools$subject <- 'pool'
  pools$timepoint <- NA
  pools$intervention <- 'pool'
  data <- bind_rows(nonpools,pools)
  
  intdata <- data.frame(int)
  datan <- bind_cols(data,intdata)
  datan1 <- datan %>% gather(feature,intensity,6:ncol(datan))
  
  ##write rt and mz
  rtdata <- rt %>% data.frame() %>% gather(feature, rt, 1: ncol(rt))
  mzdata <- t(mz) %>% data.frame() %>% gather(feature, mz, 1: nrow(mz))
  
  datan2 <- left_join(datan1,rtdata,by='feature') %>% left_join(mzdata,by='feature') %>% mutate(polarity)
  return(datan2)}

urine_pos_sel <- m2r(path,polarity)

boxplot_intervention <- function(feature=...){
  feature1 <- feature
  data1<- urine_neg_sel %>% filter(feature==feature1)
  rt<-as.character(round(data1$rt[1],digits = 2))
  mz<-data1$mz[1]
  fig <- urine_neg_sel %>% filter(feature==feature1) %>% ggplot(aes(x=intervention,y=intensity)) +
  geom_point() +
  geom_boxplot() +
  ggtitle(paste('rt=',rt,'  ','mz=',mz,' ','feature=',feature1))
  return(fig)
  
  }

featurex <- paste('X',1:86,sep='')

boxplot_intervention('X8')
figure <- lapply(featurex,boxplot_intervention)

path1 <- paste0(getwd(),'/figure/','figure',1:86,'.pdf')

boxplot_intervention_save <- function(feature=...){
  feature1 <- feature
  data1<- urine_neg_sel %>% filter(feature==feature1)
  rt<-as.character(round(data1$rt[1],digits = 2))
  mz<-data1$mz[1]
  pos<-data1$polarity[1]
  urine_neg_sel %>% filter(feature==feature1) %>% ggplot(aes(x=intervention,y=intensity)) +
    geom_point() +
    geom_boxplot() +
    ggtitle(paste('rt=',rt,'  ','mz=',mz,' ','feature=',feature1,'polarity=',pos))
  ggsave(paste0(feature1,'.jpg'))
}
figure <- lapply(featurex,boxplot_intervention_save)

#extract mz and rt
  path <- 'c://Users/tuhu/projects/barley/urine_pos_varsel.mat'
  matdata1 <- readMat(path)
  matdata <- matdata1[[1]]  
  mz_sel1 <- matdata$axisscale[[2]]
    mz_sel <-  mz_sel1[[1]]
  rt_sel1 <-  matdata$axisscale[[6]]  
    rt_sel <- rt_sel1[[1]]  
  mzrt_sel <- data.frame(mz=t(mz_sel),rt=t(rt_sel))    
  
  path <- 'c://Users/tuhu/projects/barley/urine_pos_data.mat'  
  polarity <- 'pos'
  m2r <- function(path=...,polarity=...){
    matdata <- readMat(path)
    matdata1 <- matdata[[1]] #[1] and [[1]] are different
    
    #subject info
    ##extract filename
    filename1 <- matdata1$label[[1]]
    filename <- filename1[[1]]
    ##extract samplename
    samplename1 <- matdata1$label[[5]]
    samplename <- samplename1[[1]]
    ##extract subjectnumber
    subject1 <- matdata1$label[[13]]
    subject <- subject1[[1]]
    ##extract timepoint
    timepoint1 <- matdata1$label[[17]]
    timepoint <- timepoint1[[1]]
    ##extract intervention
    intervention1 <- matdata1$label[[21]]
    intervention <- intervention1[[1]]
    ##extract rt
    rt1 <- matdata1$axisscale[[6]]
    rt <- rt1[[1]]
    ##extract mz
    mz1 <- matdata1$label[[2]]
    mz <- mz1[[1]]
    ##extract groupinfo
    #group1 <- matdata1$label[[10]]
    #group <- group1[[1]]
    ##extract intensities
    int <- matdata1$data
    
    ##shape new data
    
    data <- data.frame(filename,samplename,subject,timepoint,intervention)
    pools <- data %>% filter(samplename =='pool  ')
    nonpools <- data %>% filter(samplename != 'pool  ')
    pools$subject <- 'pool'
    pools$timepoint <- NA
    pools$intervention <- 'pool'
    data <- bind_rows(nonpools,pools)
    
    intdata <- data.frame(int)
    datan <- bind_cols(data,intdata)
    datan1 <- datan %>% gather(feature,intensity,6:ncol(datan))
    
    ##write rt and mz
    rtdata <- rt %>% data.frame() %>% gather(feature, rt, 1: ncol(rt))
    mzdata <- t(mz) %>% data.frame() %>% gather(feature, mz, 1: nrow(mz))
    
    datan2 <- left_join(datan1,rtdata,by='feature') %>% left_join(mzdata,by='feature') %>% mutate(polarity)
    return(datan2)}
  urine_pos_data <- m2r(path,polarity)  
  