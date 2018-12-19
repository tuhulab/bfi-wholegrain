# load package
library(R.matlab)
library(tidyverse)

# read mat file
##indicate the mat file path
path <- 'C:\\Users\\tuhu\\projects\\barley\\POSdata.mat'

##readMat function
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

data <- data.frame(filename,samplename,subject,timepoint,intervention)

n <- length(rt)

