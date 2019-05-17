library(tidyverse)
library(readxl)


pos <-read_excel("c:/users/czw814/projects/bfi-wholegrain/r/kp_sample.xlsx",sheet = 3) %>% data.frame() 


which.max(pos[1,6:158])
top_n(pos[1,6:158],n=5,)



intensity <- pos %>% select(colnames(pos)[-c(1:5)])
t_i <- t(intensity) %>% data.frame() 


top_n(t_i,n=5,wt = X1) %>% select(X1)
top_n(t_i,n=5,wt = X2) %>% select(X2)
