library(tidyverse)
library(caret)

feature_table <- read_delim("XCMS_result/M226_barley_urine_plate_1_neg_peaklist.csv",";")


t <- 1

y <- feature_table[t,8:ncol(feature_table)] %>% as.numeric() 
y_adjust <- if_else(is.na(y),0.01,y)
x <- 1:length(y_adjust)
data <- data.frame(y_adjust,x)
fit <- lm(y~x,data)
center <- 61/2 * fit$coefficients[2] + fit$coefficients[1]
data <- mutate(data, residuals=fit$residuals, adjusted_intensity=residuals+center)
data %>% ggplot(aes(x,y)) + geom_point() + geom_abline(slope = fit$coefficients[2],intercept = fit$coefficients[1])
data %>% mutate(residuals=fit$residuals, adjusted_intensity=residuals+center) %>%
  ggplot(aes(x,adjusted_intensity)) + geom_point()+ geom_hline(yintercept=center)
