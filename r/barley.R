library(tidyverse)

barleypath <- 'C:\\Users\\tuhu\\projects\\barley\\POSdata.mat'
barley <- m2r(barleypath)

barley %>% filter(feature=='X1268') %>% ggplot(aes(x=intervention, y=intensity)) + 
  geom_point() +
  geom_boxplot()