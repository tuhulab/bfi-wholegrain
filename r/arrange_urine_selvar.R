library(tidyverse)
library(R.matlab)
library(stringr)
#extract neg data
path <- 'c://Users/tuhu/projects/barley/urine_neg_varsel.mat'
polarity <- 'neg'

#convert mat into R dataset
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
  
  #character tweaking
  intervention_new <- datan2$intervention %>% str_replace_all(' ',"")
  
  datan2 <- datan2 %>% select(-intervention) %>% mutate(intervention=intervention_new)
  return(datan2)}

urine_neg_sel <- m2r(path,polarity)

#OUTPUT FOR IDENTIFICATION
summary_neg <- urine_neg_sel %>% 
  filter(intervention %in% c('AB','AW')) %>% group_by(intervention,feature) %>%
  summarize(mean(intensity)) 

summary_neg1 <- urine_neg_sel  %>% 
  filter(intervention %in% c('AB','AW')) %>% 
  filter(feature=='X2') %>% spread(intervention,intensity) %>% group_by(subject) %>% select(subject,'AB','AW')
summary_neg1_AB <- summary_neg1 %>% select(subject,AB) %>% drop_na(AB)
summary_neg1_AW <- summary_neg1 %>% select(subject,AW) %>% drop_na(AW)
subject <- summary_neg1_AB$subject
summary_neg1_full <- full_join(summary_neg1_AB, summary_neg1_AW)
summary_neg1_full %>% ggplot(aes(x=AB,y=AW)) + geom_point()


#PLOT NEG DATA
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

#extract mz and rt from plsda modeling (NOT NECESSARY)
  #path <- 'c://Users/tuhu/projects/barley/urine_pos_varsel.mat'
  #matdata1 <- readMat(path)
  #matdata <- matdata1[[1]]  
  #mz_sel1 <- matdata$axisscale[[2]]
  #  mz_sel <-  mz_sel1[[1]]
  #rt_sel1 <-  matdata$axisscale[[6]]  
  #  rt_sel <- rt_sel1[[1]]  
  #mzrt_sel <- data.frame(mz=t(mz_sel),rt=t(rt_sel))    
  

  # arrange POS data
  
  #path <- 'c://Users/tuhu/projects/barley/urine_pos_data.mat'  
  #this data is fu**ing wrong
  path <- 'c://Users/tuhu/projects/bfi-wholegrain/matlab/urine_pos_selected.mat'
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
    mz1 <- matdata1$axisscale[[2]]
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
    mzdata <- mz %>% data.frame() %>% gather(feature, mz, 1: ncol(mz))
    
    datan2 <- left_join(datan1,rtdata,by='feature') %>% left_join(mzdata,by='feature') %>% mutate(polarity)
    return(datan2)}
  urine_pos_data <- m2r(path,polarity)  
  
  #urine_pos_sel_data <- urine_pos_data %>% filter(mz %in% mzrt_sel$mz) %>% 
  #  filter (rt %in% mzrt_sel$rt)
  #length(unique(urine_pos_sel_data$feature)) - nrow(mzrt_sel)
  #mzrt_sel <- mzrt_sel %>% mutate(feature_new=paste0('X',1:nrow(mzrt_sel)))
  #urine_pos_sel_data_arrange <- urine_pos_sel_data %>% arrange(rt,mz,intervention,intensity)
  #test <-left_join(urine_pos_sel_data_arrange,mzrt_sel,by='rt')                       
  #urine_plsda_selvar_pos <- test %>% select(-mz.y,-feature) %>% mutate (feature=feature_new, mz=mz.x) %>% select(-feature_new, -mz.x)  

  #plot
  boxplot_intervention_save <- function(feature=...){
    feature1 <- feature
    data1<- urine_pos_data %>% filter(feature==feature1)
    rt<-as.character(round(data1$rt[1],digits = 2))
    mz<-round(data1$mz[1],digits = 4)
    pos<-data1$polarity[1]
    urine_pos_data %>% filter(feature==feature1) %>% ggplot(aes(x=intervention,y=intensity)) +
      geom_point() +
      geom_boxplot() +
      ggtitle(paste('rt=',rt,'  ','mz=',mz,' ','feature=',feature1,' ','polarity=',pos))
    ggsave(paste0(feature1,'.jpg'))
  }
  
  featureX <- paste0('X',1:length(unique(urine_pos_data$feature)))
  figure <- lapply(featureX,boxplot_intervention_save)

  #select markers for WG wheat intake
  urine_pos_data %>% group_by(feature,intervention) %>% summarize(mean(intensity))
  