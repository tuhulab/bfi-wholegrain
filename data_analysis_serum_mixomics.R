#####loading dependencies#####
library(tidyverse)
library(reshape2)
library(mixOmics)

project <- "M226_barley"
sample <-"serum_plate_1"
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
cum_var <- pca$cum.var

plot(pca)
plotIndiv(pca, 
          comp=c(1,2), 
          ind.names = FALSE,
          group = intervention$intervention, legend = TRUE, title="Serum Negative Mode, PCA comp 1-2")

serum_neg <- cum_var("M226_barley_serum_plate_1_neg")
serum_pos <- cum_var("M226_barley_serum_plate_1_pos")
urine_neg <- cum_var("M226_barley_urine_plate_1_neg")
urine_pos <- cum_var("M226_barley_urine_plate_1_pos")


pca_merge <- data.frame(serum_neg=serum_neg*100,
                        serum_pos=serum_pos*100,
                        urine_neg=urine_neg*100,
                        urine_pos=urine_pos*100) %>% as_tibble() %>% mutate(PC=paste0("PC",1:10))
pca_merge_6 <- pca_merge %>% filter(PC %in% paste0("PC",1:6)) %>% 
  gather(sample,
         cum_var,
         serum_neg,serum_pos,urine_neg,urine_pos)
#pca_merge_6 %>% ggplot(aes(PC,cum_var,fill=sample)) + geom_bar(stat="identity",position="dodge") + ggtitle("cumulitive variance")
pca_merge_6 %>% ggplot(aes(x=PC,y=cum_var,color=sample,group=sample)) + geom_point() +
  geom_line() + ggtitle("Cumulative Variance% Explained by PC(1-6)") + labs(y="Cumulative Variance%")


######define variables#####
project <- "M226_barley"
sample <-"serum_plate_1"
mode <- "neg"
id <- paste(project,sample,mode,sep = "_")
data_extract <- function(id=...){
  peaklist <- file.path("XCMS_result",paste(id,"peaklist.csv",sep="_"))
  samplelist <- file.path("XCMS_result",paste(id,"samplelist.csv",sep="_"))
  dietcode <- "data/FullDietCodes.xlsx"
  data <- read_csv(peaklist) %>% 
    mutate(feature_index = paste0("Y",1:nrow(.)))
  intervention <- read_csv(samplelist) %>% mutate(sample=samplename) %>% left_join(
    readxl::read_excel(dietcode) %>% dplyr::select(sample,intervention)
  ) %>% mutate(intervention = ifelse(is.na(intervention), "Pool", intervention)) %>% dplyr::select(code, intervention)
  data_matrix <- list()
  data_matrix$X <- data %>% dplyr::select(X1:X61) %>% t
  data_matrix$Y <- read_csv(samplelist) %>% mutate(sample=samplename) %>% 
    left_join(readxl::read_excel(dietcode) %>% dplyr::select(sample,intervention)) %>% 
    mutate(intervention = ifelse(is.na(intervention), "Pool", intervention)) %>% dplyr::select(code, intervention)
  return(data_matrix)
  }

data <- data_extract(id)

data$Y$intervention <- ifelse(data$Y$intervention != "Pool","Samples","Pool")

#####PLS-DA analysis######

plsda <- plsda(data$X,data$Y$intervention,ncomp=10)
plotIndiv(plsda, comp=1:2,
          group=data$Y$intervention, ind.names = FALSE,
          ellipse=TRUE, legend=TRUE, title=paste0("PLSDA Score Plot on Serum", " (",mode,",PC 1-2",")"))


###background_analysis###
background <- background.predict(plsda, comp.predicted=2, dist = "max.dist")
plotIndiv(plsda, comp = 1:2,
          group = data$Y$intervention, ind.names = FALSE, title = "PLSDA Score Plot Including Background for Serum Negative Mode",
          legend = TRUE,  background = background)


####assess performance###
set.seed(4881)
plsda_perf <- perf(plsda, validation = "Mfold", folds = 5, 
                         progressBar = FALSE, auc = TRUE, nrepeat = 10) 
plot(plsda_perf, col = color.mixo(5:7), sd = TRUE, legend.position = "horizontal")
plsda_auc <- auroc(plsda, roc.comp = 2)


#####sPLS-DA analysis#####
list.keepX <- c(1:10,  seq(20, 100, 10))

splsda_tune <- tune.splsda(data$X, data$Y$intervention, ncomp = 1, validation = 'Mfold', folds = 5, 
                                 progressBar = TRUE, dist = 'max.dist', measure = "BER",
                                 test.keepX = list.keepX, nrepeat = 10, cpus = 2)

#####plot serum classification error#####
plsda_error <- plsda_perf$error.rate$overall %>% as_tibble() %>% 
  dplyr::select(mean=max.dist) %>% 
  mutate(sd=ifelse(plsda_perf$error.rate.sd$overall[,1]==0,0.000518, plsda_perf$error.rate.sd$overall[,1])) %>% mutate(PC=paste0("PC",1:10), sample="serum_neg")

plsda_error %>% ggplot(aes(PC,mean,group=sample)) +geom_line() + geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd), width=.2,
                                                                               position=position_dodge(.05)) + labs(y="Classification Error", title="Classification Error by Different PCs")


#######student t-test and FDR ###########
##in another document##


#######select only AW and AB######
project <- "M226_barley"
sample <-"serum_plate_1"
mode <- "neg"
id <- paste(project,sample,mode,sep = "_")
data_extract <- function(id=...){
  peaklist <- file.path("XCMS_result",paste(id,"peaklist.csv",sep="_"))
  samplelist <- file.path("XCMS_result",paste(id,"samplelist.csv",sep="_"))
  dietcode <- "data/FullDietCodes.xlsx"
  data <- read_csv(peaklist) %>% 
    mutate(feature_index = paste0("Y",1:nrow(.)))
  intervention <- read_csv(samplelist) %>% mutate(sample=samplename) %>% left_join(
    readxl::read_excel(dietcode) %>% dplyr::select(sample,intervention)
  ) %>% mutate(intervention = ifelse(is.na(intervention), "Pool", intervention)) %>% dplyr::select(code, intervention)
  data_matrix <- list()
  data_matrix$X <- data %>% dplyr::select(X1:X61) %>% t
  data_matrix$Y <- read_csv(samplelist) %>% mutate(sample=samplename) %>% 
    left_join(readxl::read_excel(dietcode) %>% dplyr::select(sample,intervention)) %>% 
    mutate(intervention = ifelse(is.na(intervention), "Pool", intervention)) %>% dplyr::select(code, intervention)
  return(data_matrix)
}

data <- data_extract(id)

index <- data$Y$intervention %in% c("AW","AB","Pool") %>% which
X <- data$X[index,]
Y <- data$Y[index,]

pca <- pca(X,ncomp = 10, center=TRUE,
           scale=TRUE)
plotIndiv(pca, 
          comp=c(1,2), 
          ind.names = FALSE,
          group = Y$intervention, legend = TRUE, title="Serum Negative Mode, PCA comp 1-2")
