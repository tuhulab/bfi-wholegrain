library(tidyverse)
library(R.matlab)

urinepospath <- 'C:\\Users\\tuhu\\projects\\barley\\POSdata.mat'
urinenegpath <- 'C:\\Users\\tuhu\\projects\\barley\\NEGdata.mat'
urinepospolarity <- 'positive'
urinenegpolarity <- 'negative'
barleypos <- m2r(path=barleypospath, polarity = barleypospolarity)
barleyneg <- m2r(path=barleynegpath, polarity = barleynegpolarity)

barleypos %>% filter(feature=='X1268') %>% ggplot(aes(x=intervention, y=intensity)) + 
  geom_point() +
  geom_boxplot()

barleyneg %>% filter(feature=='X3001') %>% ggplot(aes(x=intervention, y=intensity)) + 
  geom_point() +
  geom_boxplot()


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
