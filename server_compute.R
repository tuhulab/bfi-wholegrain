### compute from server

# library(xcms)
# library(tidyverse)
# 
# library(RODBC)
# library(kableExtra)
# library(DT)
# library(dplyr)
# library(CAMERA)
# library(commonMZ)
# library(DT)
# library(xcms)
# library(tidyverse)
# library(CAMERA)
# library(readxl)
# install.packages("RODBC")

# BiocManager::install("hmdbQuery")
library(hmdbQuery)
library(hmdbQuery)
lk1 = HmdbEntry(prefix = "http://www.hmdb.ca/metabolites/", 
                id = "HMDB0000001")
lk1

