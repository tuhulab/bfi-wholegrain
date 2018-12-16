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