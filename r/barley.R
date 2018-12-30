library(tidyverse)
library(R.matlab)

barleypospath <- 'C:\\Users\\tuhu\\projects\\barley\\POSdata.mat'
barleynegpath <- 'C:\\Users\\tuhu\\projects\\barley\\NEGdata.mat'
barleypospolarity <- 'positive'
barleynegpolarity <- 'negative'
barleypos <- m2r(path=barleypospath, polarity = barleypospolarity)
barleyneg <- m2r(path=barleynegpath, polarity = barleynegpolarity)

barleypos %>% filter(feature=='X1268') %>% ggplot(aes(x=intervention, y=intensity)) + 
  geom_point() +
  geom_boxplot()

barleyneg %>% filter(feature=='X3001') %>% ggplot(aes(x=intervention, y=intensity)) + 
  geom_point() +
  geom_boxplot()
