library(tidyverse)
library(R.matlab)

barleypath <- 'C:\\Users\\tuhu\\projects\\barley\\POSdata.mat'
barleypolarity <- 'positive'
barley <- m2r(path=barleypath, polarity = barleypolarity)

barley %>% filter(feature=='X1268') %>% ggplot(aes(x=intervention, y=intensity)) + 
  geom_point() +
  geom_boxplot()

ion <- barley %>% filter(feature=='X1268')