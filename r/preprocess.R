library(xcms)
library(faahKO)
library(RColorBrewer)
library(pander)
library(magrittr)

#path<- 'C:/Users/tuhu/projects/bfi-wholegrain/data/trail'
## Get the full path to the CDF files
path <- system.file('cdf',package='faahKO')

cdfs <- dir(path, full.names = TRUE,
            recursive = TRUE)

## Create a phenodata data.frame
pd <- data.frame(file_name = sub(basename(cdfs), pattern = "01.CDF",
                                   replacement = "", fixed = TRUE),
                 sample_group = c(rep("KO", 6), rep("WT", 6)),
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
bpi_1 <- bpis[1, 1]
head(rtime(bpi_1))
head(intensity(bpi_1))

## Get the total ion current by file
tc <- split(tic(raw_data), f = fromFile(raw_data))
boxplot(tc, col = group_colors[raw_data$sample_group],
        ylab = "intensity", main = "Total ion current")

## Define the rt and m/z range of the peak area
rtr <- c(2700, 2900)
mzr <- c(334.9, 335.1)
## extract the chromatogram
chr_raw <- chromatogram(raw_data, mz = mzr, rt = rtr)
plot(chr_raw, col = group_colors[chr_raw$sample_group])

raw_data %>%
  filterRt(rt = rtr) %>%
  filterMz(mz = mzr) %>%
  plot(type = "XIC")

cwp <- CentWaveParam(peakwidth = c(20, 80), noise = 5000)
xdata <- findChromPeaks(raw_data, param = cwp)

head(chromPeaks(xdata))

summary_fun <- function(z) {
  c(peak_count = nrow(z), rt = quantile(z[, "rtmax"] - z[, "rtmin"]))
}
T <- lapply(split.data.frame(chromPeaks(xdata),
                             f = chromPeaks(xdata)[, "sample"]),
            FUN = summary_fun)
T <- do.call(rbind, T)
rownames(T) <- basename(fileNames(xdata))
pandoc.table(T,
             caption = paste0("Summary statistics on identified chromatographic",
                              " peaks. Shown are number of identified peaks per",
                              " sample and widths/duration of chromatographic ",
                              "peaks."))
plotChromPeaks(xdata, file = 3)

plot(chr_raw, col = group_colors[chr_raw$sample_group], lwd = 2)
highlightChromPeaks(xdata, border = group_colors[chr_raw$sample_group],
                    lty = 3, rt = rtr, mz = mzr, type = "rect")
