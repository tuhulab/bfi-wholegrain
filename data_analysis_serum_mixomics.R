#####loading dependencies#####
library(tidyverse)
library(mixOmics)

project <- "M226_barley"
sample <-"urine_plate_1"
mode <- "neg"
id <- paste(project,sample,mode,sep = "_")

peaklist <- file.path("XCMS_result",paste(id,"peaklist.csv",sep="_"))
samplelist <- file.path("XCMS_result",paste(id,"samplelist.csv",sep="_"))
dietcode <- "data/FullDietCodes.xlsx"

data <- read_csv(peaklist) %>% 
        mutate(feature_index = paste0("Y",1:nrow(.)))

intervention <- read_csv(samplelist) %>% mutate(sample=samplename) %>% left_join(
  readxl::read_excel(dietcode) %>% dplyr::select(sample,intervention)
) %>% mutate(intervention = ifelse(is.na(intervention), "Pool", intervention)) %>% dplyr::select(code, intervention)

data_matrix <- data %>% dplyr::select(X1:X61) %>% t

pca <- mixOmics::pca(data_matrix,
                     ncomp = 10,
                     center = TRUE,
                     scale = TRUE)
plot(pca)
plotIndiv(pca, 
          comp=c(1,2), 
          ind.names = TRUE,
          group = intervention$intervention, legend = TRUE, title="serum pos, PCA comp 1-2")
