library(xcms)
library(faahKO)
library(RColorBrewer)
library(pander)
library(magrittr)

path<- 'C:/Users/tuhu/projects/bfi-wholegrain/data/trail'
## Get the full path to the CDF files
cdfs <- dir(path, full.names = TRUE,
            recursive = TRUE)

## Create a phenodata data.frame
pd <- data.frame(file_name = sub(basename(cdfs), pattern = "01.CDF",
                                   replacement = "", fixed = TRUE),
                 #sample_group = c(rep("KO", 6), rep("WT", 6)),
                 stringsAsFactors = FALSE)

raw_data <- readMSData(files = cdfs, pdata = new("NAnnotatedDataFrame", pd),
                       mode = "onDisk")

head(rtime(raw_data))
mzs <- mz(raw_data)
mzs_by_file <- split(mzs, f = fromFile(raw_data))
length(mzs_by_file)

## Get the base peak chromatograms. This reads data from the files.
bpis <- chromatogram(raw_data, aggregationFun = "max")
## Define colors for the two groups
group_colors <- paste0(brewer.pal(3, "Set1")[1:2], "60")
names(group_colors) <- c("KO", "WT")

## Plot all chromatograms.
plot(bpis, col = group_colors[raw_data$sample_group])


## Define the rt and m/z range of the peak area
rtr <- c(0, 7)
mzr <- c(0, 1000)
## extract the chromatogram
chr_raw <- chromatogram(raw_data, mz = mzr, rt = rtr)
plot(chr_raw)
