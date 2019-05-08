library(tidyr)
library(R.matlab)
library(tidyverse)

urinepospath <- 'C:\\Users\\czw814\\projects\\bfi-wholegrain\\matlab\\urine_pos_data.mat'
urinenegpath <- 'C:\\Users\\czw814\\projects\\bfi-wholegrain\\matlab\\urine_neg_data.mat'
urinepospolarity <- 'positive'
urinenegpolarity <- 'negative'

m2r <- function(path=...,polarity=...) {
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
  rt1 <- matdata1$label[[2]]
  rt <- rt1[[1]]
  ##extract mz
  mz1 <- matdata1$label[[6]]
  mz <- mz1[[1]]
  ##extract groupinfo
  group1 <- matdata1$label[[10]]
  group <- group1[[1]]
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
  
  ####### error is here!!!!!!! sequence is changed!!!!!
  intdata <- data.frame(int)
  datan <- bind_cols(data,intdata)
  datan1 <- datan %>% gather(feature,intensity,6:ncol(datan))
  
  ##write rt and mz
  rtdata <- t(rt) %>% data.frame() %>% gather(feature, rt, 1: nrow(rt))
  mzdata <- t(mz) %>% data.frame() %>% gather(feature, mz, 1: nrow(mz))
  
  datan2 <- left_join(datan1,rtdata,by='feature') %>% left_join(mzdata,by='feature') %>% mutate(polarity)
  return(datan2)
}
barleypos <- m2r(path=urinepospath, polarity = urinepospolarity)
barleyneg <- m2r(path=urinenegpath, polarity = urinenegpolarity)

barleypos %>% filter(feature=='X1268') %>% ggplot(aes(x=intervention, y=intensity)) + 
  geom_point() +
  geom_boxplot()

barleyneg %>% filter(feature=='X3001') %>% ggplot(aes(x=intervention, y=intensity)) + 
  geom_point() +
  geom_boxplot()

barleyneg %>% filter(feature=='X1123') %>% ggplot(aes(x=intervention, y=intensity)) + 
  geom_point() +
  geom_boxplot()
#search ion
  #define ion
    target_rt_1 <- 3.99
    target_mz_1 <- 517.303
  
    target_rt_2 <- 4.22
    target_mz_2 <- 501.308

    rt_window <- 0.02
    mz_window <- 0.05
    
    barleyneg %>% filter(rt>target_rt_2-rt_window & rt<target_rt_2+rt_window) %>% 
      filter(mz>target_mz_2-mz_window & mz<target_mz_2+mz_window) 
    
    
serumnegpath <- 'C:\\Users\\tuhu\\projects\\bfi-wholegrain\\matlab\\serum_neg.mat'
serumnegpolarity <- 'negative'
serumneg <- m2r(path=serumnegpath,polarity=serumnegpolarity)
serumneg %>% filter(feature=='X1205') %>% ggplot(aes(x=intervention, y=intensity,title='hi')) + 
  geom_point() +
  geom_boxplot() +
  ggtitle('AR 21:0')
serumneg %>% filter(feature=='X1199') %>% ggplot(aes(x=intervention, y=intensity)) + 
  geom_point() +
  geom_boxplot() +
  ggtitle('AR 21:1')
serumneg %>% filter(feature=='X1064') %>% ggplot(aes(x=intervention, y=intensity)) + 
  geom_point() +
  geom_boxplot() +
  ggtitle('AR 19:0')

urine_wgpath <- 'C:\\Users\\tuhu\\projects\\bfi-wholegrain\\matlab\\urine_wholegrainmarker.mat'
urine_wgpolarity <- 'negative'
urine_wg <- m2r(path=urine_wgpath, polarity=urine_wgpolarity)
urine_wg %>% filter(feature=='X1') %>% ggplot(aes(x=intervention, y=intensity,title='hi')) + 
  geom_point() +
  geom_boxplot() +
  ggtitle('3,5 DHPPA glucuronide')
urine_wg %>% filter(feature=='X3') %>% ggplot(aes(x=intervention, y=intensity,title='hi')) + 
  geom_point() +
  geom_boxplot() +
  ggtitle('3,5 DHBA glucuronide')
urine_wg %>% filter(feature=='X4') %>% ggplot(aes(x=intervention, y=intensity,title='hi')) + 
  geom_point() +
  geom_boxplot() +
  ggtitle('3,5 DHPPA')
urine_wg %>% filter(feature=='X5') %>% ggplot(aes(x=intervention, y=intensity,title='hi')) + 
  geom_point() +
  geom_boxplot() +
  ggtitle('3,5 DHBA')

path <- 'C:\\Users\\tuhu\\projects\\bfi-wholegrain\\matlab\\urine_pos_selected.mat'
polarity <- 'positive'
wheatpos <- m2r(path=path, polarity = polarity)
wheatpos %>% filter(feature=='X13') %>% ggplot(aes(x=intervention, y=intensity)) + 
  geom_point() +
  geom_boxplot() 

