##LOAD DEPENDENCIES
library(RODBC)
library(kableExtra)
library(DT)
library(dplyr)
library(CAMERA)
library(commonMZ)
library(DT)
library(xcms)
library(tidyverse)
library(CAMERA)
library(readxl)

####
##Read spl##

project_name <- "M226_barley" #upper-case
analysis_name <- "serum_plate_1" #lower-case
mode <- "neg" #"pos"or"neg"
id <- paste(project_name,analysis_name,mode,sep = "_")
id_1 <- paste(project_name,analysis_name,sep = "_")
mzML_dir <- "data/BARLEY_serum_raw/Data/"

waters_extract_spl <- function(file){
  require(RODBC)
  con <- odbcConnectAccess2007(file)
  if(con==-1) return(data_frame())
  out <- sqlFetch(con, "ANALYSIS", stringsAsFactors = FALSE) %>%
    mutate_at(vars(one_of('SAMPLE_LOCATION', 'FILE_NAME')), funs(as.character)) %>%
    mutate_at(vars(contains('CONC_')), funs(as.character))
  close(con)
  return(out)}

spl <- waters_extract_spl(file.path("spl",paste0(id_1,".SPL"))) %>%
  select(samplename=FILE_TEXT, filename=FILE_NAME, polarity=MS_FILE) %>% 
  as_tibble()

###tweek polarity###
polarity <- spl$polarity
polarity[str_which(polarity,"pos")] <- "pos"
polarity_1 <- ifelse(polarity=="pos","pos","neg")


spl_1 <- spl %>% mutate(polarity=polarity_1) %>% filter(samplename != "")

spl_mode <- spl_1 %>% 
  filter(polarity == mode,
         samplename != "Blank",
         samplename != "Blank+IntStd",
         samplename != "MetStd",
         samplename != "MetStd+IntStd") %>% mutate(sample_group = "one")

cdfs <- file.path(mzML_dir,paste0(spl_mode$filename,".mzML"))
file.exists(cdfs)
#########define parameters##########
params <- list()
params$CentWave <- CentWaveParam( ppm = 30,
                                  peakwidth =  c(0.025*60,0.30*60), # changed it to half minute  #shorteni previous one as well
                                  snthresh = 10,
                                  noise = 0,
                                  prefilter = c(3,20), #I changed this parameter. 1E3 seems too strict for untargeted metabolomics
                                  integrate = 2,
                                  mzdiff = -0.001,
                                  verboseColumns = TRUE,
                                  fitgauss = TRUE)
params$group1 <- PeakDensityParam( sampleGroups = spl_mode$sample_group, 
                                   binSize = 0.01, #in Da 
                                   bw = 0.2*60,
                                   minSamples = 1,#critical param
                                   minFraction = 0.15,#critical param
                                   maxFeatures = 10) # Jan set it to 20
params$PeakAlign <- PeakGroupsParam(  smooth = "loess",
                                      span = 0.6,
                                      minFraction= 0.9,
                                      family = "gaussian",
                                      extraPeaks = 3)

params$PeakAlign2 <- PeakGroupsParam( smooth = "loess",
                                      span = 0.6,
                                      minFraction    = 0.9,
                                      family = "gaussian",
                                      extraPeaks = 3)
mz_window <- 0.015
rt_window <- 10
###########read raw data############
raw <- readMSData(cdfs, pdata = new("NAnnotatedDataFrame",spl_mode), mode = "onDisk", msLevel. = 1)
##########peak picking##############
xset <- findChromPeaks(raw, param = params$CentWave)
###########first time group###############
xset_g <- groupChromPeaks(xset, param = params$group1)
xset_g_r <- adjustRtime(xset_g, param = params$PeakAlign)


##########adjust r time###############
plotAdjustedRtime(xset_g_r, 
                  col = as.factor(pull(pData(xset_g_r), 
                                       sample_group)), 
                  peakGroupsCol = "grey", 
                  peakGroupsPch = 1)
######second time visualization#######


params$group2 <- PeakDensityParam( sampleGroups = xset_g_r %>% pData %>% pull(sample_group), 
                                   binSize = 0.1, 
                                   bw = 0.1*60,
                                   minSamples = 1,
                                   minFraction = 0.20,
                                   maxFeatures = 5)


xset_g_r_g <- groupChromPeaks(xset_g_r, param = params$group2)
xset_g_r_g_r <- adjustRtime(applyAdjustedRtime(xset_g_r_g), param = params$PeakAlign2) 
plotAdjustedRtime(xset_g_r_g_r, col = as.factor(pull(pData(xset_g_r_g_r), sample_group)), peakGroupsCol = "grey", peakGroupsPch = 1)

######peak filling########
params$FillChromPeaks <- FillChromPeaksParam(expandMz = 0, expandRt = 0, ppm = 30)
xset_g_r_g_fill <- fillChromPeaks(xset_g_r_g, params$FillChromPeaks, BPPARAM = SnowParam(4))

peaks_aligned <- featureValues( xset_g_r_g_fill, 
                                value = "into",
                                method = "medret",
                                intensity = "into", 
                                filled = TRUE,
                                missing = NA)
kable(peaks_aligned[1:5,], format = "html") %>% kable_styling(font_size = 9)
feature_def <- featureDefinitions(xset_g_r_g_fill)
datatable(as_tibble(feature_def)) %>% 
  formatRound(columns=c("mzmed", "mzmin", "mzmax"), digits=4) %>% 
  formatRound(columns=c("rtmed", "rtmin", "rtmax"), digits=2)

################write################
# urine_pos_csv <- featureDefinitions( xset_g_r_g_fill) %>% 
# as_tibble %>% 
# select(-peakidx) %>% 
# bind_cols(as_tibble(peaks_aligned))

# write_csv(urine_pos_csv,
#            path = "c://Users//czw814//Desktop//urine_pos_csv_1.csv")
# 
################write################

xsa <- xsAnnotate(as(xset_g_r_g_fill, "xcmsSet"), sample=NA, nSlaves = detectCores()-1, polarity = 'positive')
xsaF <- groupFWHM(xsa, perfwhm =0.1, intval = "into", sigma = 6)
xsaC <-  groupCorr(xsaF,
                   calcIso = FALSE, 
                   calcCiS = TRUE, 
                   calcCaS = TRUE, 
                   cor_eic_th=0.7,
                   cor_exp_th=0.7,
                   pval= 0.000001, 
                   graphMethod="lpc",
                   intval="into")
xsaFI <- findIsotopes(xsaC, ppm = 10, mzabs= 0.01,  intval = "into")
rules <- MZ_CAMERA(mode = mode, warn_clash = TRUE, clash_ppm = 5)
rules <- as.data.frame(rules)
# rules
# 
# rules %>% datatable(filter = "top", rownames = FALSE, options = list(sDom  = '<"top">lrt<"bottom">ip')) %>% 
#     formatStyle(., columns = 1:ncol(rules), fontSize = '75%', padding="0")
# 

rules_count <- 
  xsaFA@annoID[, "ruleID"] %>% 
  factor(levels=1:nrow(rules)) %>% 
  table %>% as.matrix %>% as.data.frame %>% 
  bind_cols(rules[,'name',drop=FALSE],.) %>% 
  setNames(c("Rule","Count")) %>% 
  arrange(desc(Count)) %>% 
  as.data.frame
# pie(as.numeric(rules_count[,"Count"]),labels=rules_count[,"Rule"])

xsaFA <- findAdducts(xsaFI, ppm=10, mzabs=0.01, multiplier=4, polarity=mode, rules=rules)
peaklist <- getPeaklist(xsaFA)

peaklist %>% select("mz", "rt", "isotopes", "adduct", "pcgroup") %>% 
  kable(format="html", padding=0) %>% 
  kable_styling(font_size = 10)

pos_peaklist <- peaklist %>% 
  as_tibble %>% select(-one)

mz <- round(pos_peaklist$mz,4)
mzmin <-round(pos_peaklist$mzmin,4)
mzmax <-round(pos_peaklist$mzmax,4)
rt <- round(pos_peaklist$rt/60,2)
rtmin <- round(pos_peaklist$rtmin/60,2)
rtmax <- round(pos_peaklist$rtmax/60,2)
data_output <- data.frame(mz,mzmin,mzmax,rt,rtmin,rtmax) %>% bind_cols(pos_peaklist[,7:ncol(pos_peaklist)]) %>% select(-npeaks) %>% as_tibble()

data_output_deisotope <- data_output %>% deisotope()

data_output_annotation <- annotate_kudb(mz_window,rt_window/60,polarity = mode,data = data_output_deisotope)

write_csv(data_output_annotation,
          path = file.path("xcms_result",paste(id,"peaklist.csv",sep = "_")))
write_csv(spl_mode %>% select(-sample_group) %>% mutate(code=paste0("X",1:nrow(.))),
          path = file.path("xcms_result",paste(id,"samplelist.csv",sep = "_")))


